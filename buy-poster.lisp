(defun new-firefractal-order (first-name last-name address city state zip email)
  (let ((order-email-message ""))
    
  ))

(define-easy-handler (buy-poster
                      :uri "/buy-poster/"
                      :default-request-type :post)
  (shipping-first-name shipping-last-name shipping-address shipping-city shipping-state shipping-zip shipping-email)
  (new-firefractal-order shipping-first-name shipping-last-name shipping-address shipping-city shipping-state shipping-zip shipping-email)
  )