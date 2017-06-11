package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"strings"
	"time"

	"bitbucket.org/spyrosoft/firefractal"
	"github.com/julienschmidt/httprouter"
)

type CachedImage struct {
	Data     []byte
	Accessed time.Time
}

var (
	imageCache = make(map[string]CachedImage)
)

type FractalSettings struct {
	Zoom           uint64     `json:"zoom-level"`
	X              float64    `json:"x"`
	Y              float64    `json:"y"`
	CanvasWidth    uint64     `json:"canvas-width"`
	CanvasHeight   uint64     `json:"canvas-height"`
	PNGWidth       uint64     `json:"png-width"`
	PNGHeight      uint64     `json:"png-height"`
	MaxIterations  uint16     `json:"max-iterations"`
	GradientColors [][3]uint8 `json:"gradient-colors"`
	Fractal        string     `json:"fractal"`
	Fine           bool       `json:"fine"`
}

func png(responseWriter http.ResponseWriter, request *http.Request, requestParameters httprouter.Params) {
	err := validatePngParameters(request)
	if err != nil {
		fmt.Fprint(responseWriter, err.Error())
		return
	}
	settings := pngFractalSettings(request)
	settingsJSON, _ := json.Marshal(settings)
	var pngBytes []byte
	cachedImage, ok := imageCache[string(settingsJSON)]
	if !ok {
		var buf bytes.Buffer
		err = firefractal.PNG(settingsJSON, &buf)
		pngBytes = buf.Bytes()
		limitCacheSize()
		imageCache[string(settingsJSON)] = CachedImage{pngBytes, time.Now()}
	} else {
		pngBytes = cachedImage.Data
		cachedImage.Accessed = time.Now()
	}
	io.Copy(responseWriter, bytes.NewReader(pngBytes))
	if err != nil {
		fmt.Fprint(responseWriter, "Something went wrong:"+err.Error())
	}
}

func validatePngParameters(request *http.Request) (err error) {
	var errorMessages = []string{}
	if siteData.LiveOrDev != "dev" {
		canvasWidth, _ := strconv.Atoi(request.FormValue("canvas-width"))
		canvasHeight, _ := strconv.Atoi(request.FormValue("canvas-width"))
		if canvasWidth > 800 {
			errorMessages = append(errorMessages, "Image width is limited to 800.")
			return
		}
		if canvasHeight > 800 {
			errorMessages = append(errorMessages, "Image height is limited to 800.")
			return
		}
	}
	integersToValidate := []string{
		"zoom-level",
		"canvas-width",
		"canvas-height",
		"max-iterations",
	}
	if request.FormValue("png-width") != "" {
		integersToValidate = append(integersToValidate, "png-width")
	}
	if request.FormValue("png-height") != "" {
		integersToValidate = append(integersToValidate, "png-height")
	}
	floatsToValidate := []string{
		"x",
		"y",
	}
	stringsToValidate := []string{
		"fractal",
		"gradient-colors",
	}

	for _, integer := range integersToValidate {
		_, err := strconv.Atoi(request.FormValue(integer))
		if err != nil {
			errorMessages = append(errorMessages, "The \""+integer+"\" value must be an integer.")
		}
	}

	for _, float := range floatsToValidate {
		_, err := strconv.ParseFloat(request.FormValue(float), 64)
		if err != nil {
			errorMessages = append(errorMessages, "The \""+float+"\" value must be a number.")
		}
	}

	for _, string := range stringsToValidate {
		if request.FormValue(string) == "" {
			errorMessages = append(errorMessages, "The \""+string+"\" value is required.")
		}
	}

	gradientColors := request.FormValue("gradient-colors")
	_, err = hexToRgb(gradientColors)
	if gradientColors == "" || len(gradientColors)%6 != 0 || err != nil {
		errorMessages = append(errorMessages, "The \"gradient-colors\" value must be a string of hex values some factor of six.")
	}

	if len(errorMessages) > 0 {
		err = errors.New(strings.Join(errorMessages, "\n"))
	}
	return
}

func pngFractalSettings(request *http.Request) (settings FractalSettings) {
	settings.Zoom, _ = strconv.ParseUint(request.FormValue("zoom-level"), 10, 64)
	settings.X, _ = strconv.ParseFloat(request.FormValue("x"), 64)
	settings.Y, _ = strconv.ParseFloat(request.FormValue("y"), 64)
	settings.CanvasWidth, _ = strconv.ParseUint(request.FormValue("canvas-width"), 10, 64)
	settings.CanvasHeight, _ = strconv.ParseUint(request.FormValue("canvas-height"), 10, 64)
	if request.FormValue("png-width") != "" {
		settings.PNGWidth, _ = strconv.ParseUint(request.FormValue("png-width"), 10, 64)
	}
	if request.FormValue("png-height") != "" {
		settings.PNGHeight, _ = strconv.ParseUint(request.FormValue("png-height"), 10, 64)
	}
	maxIterations, _ := strconv.ParseUint(request.FormValue("max-iterations"), 10, 16)
	settings.MaxIterations = uint16(maxIterations)
	settings.Fractal = request.FormValue("fractal")
	settings.GradientColors, _ = hexToRgb(request.FormValue("gradient-colors"))
	settings.Fine = true
	if request.FormValue("fine") == "false" {
		settings.Fine = false
	}
	return
}

func hexToRgb(sixHex string) (rgbs [][3]uint8, err error) {
	for i := 0; i < len(sixHex); i += 6 {
		var r, g, b int64
		r, err = strconv.ParseInt(sixHex[i:i+2], 16, 16)
		if err != nil {
			return
		}
		g, err = strconv.ParseInt(sixHex[i+2:i+4], 16, 16)
		if err != nil {
			return
		}
		b, err = strconv.ParseInt(sixHex[i+4:i+6], 16, 16)
		if err != nil {
			return
		}
		rgb := [3]uint8{uint8(r), uint8(g), uint8(b)}
		rgbs = append(rgbs, rgb)
	}
	return
}

func limitCacheSize() {
	for {
		if len(imageCache) <= 500 {
			return
		}
		var oldestImageKey string
		var oldestImageTime time.Time
		for key, cachedImage := range imageCache {
			if cachedImage.Accessed.Before(oldestImageTime) {
				oldestImageKey = key
				oldestImageTime = cachedImage.Accessed
			}
		}
		delete(imageCache, oldestImageKey)
	}
}
