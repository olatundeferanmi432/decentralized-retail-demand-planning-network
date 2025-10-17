;; Demand Planning Coordinator Contract
;; Coordinate retail demand planning and forecasting processes, analyze sales patterns,
;; predict seasonal trends, optimize inventory levels, and improve demand accuracy

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u700))
(define-constant ERR_PRODUCT_NOT_FOUND (err u701))
(define-constant ERR_FORECAST_NOT_FOUND (err u702))
(define-constant ERR_INVALID_PARAMETERS (err u703))
(define-constant ERR_INSUFFICIENT_DATA (err u704))
(define-constant ERR_TREND_NOT_FOUND (err u705))
(define-constant FORECAST_ACCURACY_THRESHOLD u80) ;; 80% minimum accuracy
(define-constant MAX_FORECAST_PERIOD u365) ;; Maximum 365 days

;; Data Variables
(define-data-var next-product-id uint u1)
(define-data-var next-forecast-id uint u1)
(define-data-var next-trend-id uint u1)
(define-data-var contract-admin principal CONTRACT_OWNER)
(define-data-var total-products uint u0)
(define-data-var total-forecasts uint u0)
(define-data-var demand-planning-enabled bool true)

;; Utility Functions
(define-private (min-uint (a uint) (b uint))
    (if (< a b) a b)
)

(define-private (max-uint (a uint) (b uint))
    (if (> a b) a b)
)

(define-private (calculate-percentage (amount uint) (total uint))
    (if (> total u0)
        (/ (* amount u100) total)
        u0
    )
)

;; Data Maps
(define-map retail-products
    uint
    {
        product-id: uint,
        product-name: (string-ascii 200),
        category: (string-ascii 100),
        brand: (string-ascii 100),
        sku: (string-ascii 50),
        price: uint,
        seasonality-factor: uint,
        demand-variability: uint,
        lead-time-days: uint,
        supplier-count: uint,
        current-inventory: uint,
        safety-stock-level: uint,
        reorder-point: uint,
        economic-order-quantity: uint,
        last-demand-update: uint
    }
)

(define-map demand-forecasts
    uint
    {
        forecast-id: uint,
        product-id: uint,
        forecast-period-days: uint,
        start-date: uint,
        end-date: uint,
        predicted-demand: uint,
        confidence-level: uint,
        seasonality-adjustment: uint,
        trend-adjustment: uint,
        external-factors: (string-ascii 200),
        forecasting-model: (string-ascii 50),
        created-by: principal,
        accuracy-score: uint,
        actual-demand: uint,
        status: (string-ascii 20)
    }
)

(define-map sales-patterns
    { product-id: uint, period: uint }
    {
        sales-volume: uint,
        sales-value: uint,
        period-start: uint,
        period-end: uint,
        channel-mix: (string-ascii 100),
        geographic-mix: (string-ascii 100),
        promotional-impact: uint,
        weather-impact: uint,
        competitive-impact: uint,
        trend-direction: (string-ascii 20)
    }
)

(define-map seasonal-trends
    uint
    {
        trend-id: uint,
        product-category: (string-ascii 100),
        seasonal-pattern: (string-ascii 50),
        peak-periods: (list 12 uint),
        low-periods: (list 12 uint),
        trend-strength: uint,
        historical-data-points: uint,
        confidence-score: uint,
        last-updated: uint,
        analyst: principal
    }
)

(define-map inventory-recommendations
    uint
    {
        product-id: uint,
        recommendation-type: (string-ascii 50),
        current-stock: uint,
        recommended-stock: uint,
        reorder-quantity: uint,
        reorder-timing: uint,
        safety-stock-adjustment: uint,
        lead-time-consideration: uint,
        seasonal-adjustment: uint,
        confidence-level: uint,
        cost-impact: uint,
        generated-date: uint
    }
)

(define-map demand-analytics
    uint
    {
        product-id: uint,
        demand-volatility: uint,
        forecast-accuracy: uint,
        seasonal-index: uint,
        trend-coefficient: uint,
        demand-correlation: uint,
        stockout-frequency: uint,
        overstock-frequency: uint,
        turnover-rate: uint,
        fill-rate: uint,
        last-calculated: uint
    }
)

(define-map planning-performance
    { planner: principal, period: uint }
    {
        forecasts-created: uint,
        accuracy-rate: uint,
        inventory-optimization: uint,
        cost-savings: uint,
        stockout-prevention: uint,
        overstock-reduction: uint,
        performance-score: uint,
        period-start: uint,
        period-end: uint
    }
)

(define-map authorized-planners principal 
    {
        name: (string-ascii 100),
        specialization: (string-ascii 100),
        experience-level: uint,
        accuracy-rating: uint,
        products-managed: uint,
        is-active: bool
    }
)

;; Authorization Functions
(define-private (is-authorized (user principal))
    (or 
        (is-eq user (var-get contract-admin))
        (is-some (map-get? authorized-planners user))
    )
)

(define-public (register-demand-planner
    (planner principal)
    (name (string-ascii 100))
    (specialization (string-ascii 100))
    (experience-level uint)
)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR_UNAUTHORIZED)
        (asserts! (<= experience-level u5) ERR_INVALID_PARAMETERS)
        
        (map-set authorized-planners planner {
            name: name,
            specialization: specialization,
            experience-level: experience-level,
            accuracy-rating: u50,
            products-managed: u0,
            is-active: true
        })
        (ok true)
    )
)

;; Product Management
(define-public (register-product
    (product-name (string-ascii 200))
    (category (string-ascii 100))
    (brand (string-ascii 100))
    (sku (string-ascii 50))
    (price uint)
    (seasonality-factor uint)
    (demand-variability uint)
    (lead-time-days uint)
    (supplier-count uint)
)
    (let
        (
            (product-id (var-get next-product-id))
        )
        (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
        (asserts! (> price u0) ERR_INVALID_PARAMETERS)
        (asserts! (<= seasonality-factor u100) ERR_INVALID_PARAMETERS)
        
        (map-set retail-products product-id {
            product-id: product-id,
            product-name: product-name,
            category: category,
            brand: brand,
            sku: sku,
            price: price,
            seasonality-factor: seasonality-factor,
            demand-variability: demand-variability,
            lead-time-days: lead-time-days,
            supplier-count: supplier-count,
            current-inventory: u0,
            safety-stock-level: u0,
            reorder-point: u0,
            economic-order-quantity: u0,
            last-demand-update: (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))
        })
        
        (var-set next-product-id (+ product-id u1))
        (var-set total-products (+ (var-get total-products) u1))
        (ok product-id)
    )
)

;; Demand Forecasting
(define-public (create-demand-forecast
    (product-id uint)
    (forecast-period-days uint)
    (predicted-demand uint)
    (confidence-level uint)
    (seasonality-adjustment uint)
    (trend-adjustment uint)
    (external-factors (string-ascii 200))
    (forecasting-model (string-ascii 50))
)
    (let
        (
            (forecast-id (var-get next-forecast-id))
            (product (unwrap! (map-get? retail-products product-id) ERR_PRODUCT_NOT_FOUND))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (end-date (+ current-time (* forecast-period-days u86400)))
            (planner (unwrap! (map-get? authorized-planners tx-sender) ERR_UNAUTHORIZED))
        )
        (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
        (asserts! (<= forecast-period-days MAX_FORECAST_PERIOD) ERR_INVALID_PARAMETERS)
        (asserts! (<= confidence-level u100) ERR_INVALID_PARAMETERS)
        
        (map-set demand-forecasts forecast-id {
            forecast-id: forecast-id,
            product-id: product-id,
            forecast-period-days: forecast-period-days,
            start-date: current-time,
            end-date: end-date,
            predicted-demand: predicted-demand,
            confidence-level: confidence-level,
            seasonality-adjustment: seasonality-adjustment,
            trend-adjustment: trend-adjustment,
            external-factors: external-factors,
            forecasting-model: forecasting-model,
            created-by: tx-sender,
            accuracy-score: u0,
            actual-demand: u0,
            status: "active"
        })
        
        ;; Update planner statistics
        (map-set authorized-planners tx-sender (merge planner {
            products-managed: (+ (get products-managed planner) u1)
        }))
        
        (var-set next-forecast-id (+ forecast-id u1))
        (var-set total-forecasts (+ (var-get total-forecasts) u1))
        (ok forecast-id)
    )
)

;; Sales Pattern Recording
(define-public (record-sales-pattern
    (product-id uint)
    (period uint)
    (sales-volume uint)
    (sales-value uint)
    (channel-mix (string-ascii 100))
    (geographic-mix (string-ascii 100))
    (promotional-impact uint)
    (weather-impact uint)
    (competitive-impact uint)
)
    (let
        (
            (product (unwrap! (map-get? retail-products product-id) ERR_PRODUCT_NOT_FOUND))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (period-start (- current-time (* period u86400)))
            (trend-direction (if (> sales-volume (/ (get current-inventory product) u2)) "increasing" "decreasing"))
        )
        (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
        (asserts! (> sales-volume u0) ERR_INVALID_PARAMETERS)
        
        (map-set sales-patterns { product-id: product-id, period: period } {
            sales-volume: sales-volume,
            sales-value: sales-value,
            period-start: period-start,
            period-end: current-time,
            channel-mix: channel-mix,
            geographic-mix: geographic-mix,
            promotional-impact: promotional-impact,
            weather-impact: weather-impact,
            competitive-impact: competitive-impact,
            trend-direction: trend-direction
        })
        
        ;; Update product last demand update
        (map-set retail-products product-id (merge product {
            last-demand-update: current-time
        }))
        
        (ok true)
    )
)

;; Seasonal Trend Analysis
(define-public (create-seasonal-trend
    (product-category (string-ascii 100))
    (seasonal-pattern (string-ascii 50))
    (peak-periods (list 12 uint))
    (low-periods (list 12 uint))
    (trend-strength uint)
    (historical-data-points uint)
)
    (let
        (
            (trend-id (var-get next-trend-id))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (confidence-score (min-uint u100 (/ (* trend-strength historical-data-points) u100)))
        )
        (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
        (asserts! (<= trend-strength u100) ERR_INVALID_PARAMETERS)
        (asserts! (> historical-data-points u0) ERR_INVALID_PARAMETERS)
        
        (map-set seasonal-trends trend-id {
            trend-id: trend-id,
            product-category: product-category,
            seasonal-pattern: seasonal-pattern,
            peak-periods: peak-periods,
            low-periods: low-periods,
            trend-strength: trend-strength,
            historical-data-points: historical-data-points,
            confidence-score: confidence-score,
            last-updated: current-time,
            analyst: tx-sender
        })
        
        (var-set next-trend-id (+ trend-id u1))
        (ok trend-id)
    )
)

;; Inventory Optimization
(define-public (generate-inventory-recommendation
    (product-id uint)
    (recommendation-type (string-ascii 50))
    (recommended-stock uint)
    (reorder-quantity uint)
    (reorder-timing uint)
    (safety-stock-adjustment uint)
    (seasonal-adjustment uint)
)
    (let
        (
            (product (unwrap! (map-get? retail-products product-id) ERR_PRODUCT_NOT_FOUND))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (cost-impact (* reorder-quantity (get price product)))
            (confidence-level u85)
        )
        (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
        (asserts! (> recommended-stock u0) ERR_INVALID_PARAMETERS)
        
        (map-set inventory-recommendations product-id {
            product-id: product-id,
            recommendation-type: recommendation-type,
            current-stock: (get current-inventory product),
            recommended-stock: recommended-stock,
            reorder-quantity: reorder-quantity,
            reorder-timing: reorder-timing,
            safety-stock-adjustment: safety-stock-adjustment,
            lead-time-consideration: (get lead-time-days product),
            seasonal-adjustment: seasonal-adjustment,
            confidence-level: confidence-level,
            cost-impact: cost-impact,
            generated-date: current-time
        })
        
        ;; Update product inventory parameters
        (map-set retail-products product-id (merge product {
            safety-stock-level: (+ (get safety-stock-level product) safety-stock-adjustment),
            reorder-point: reorder-timing,
            economic-order-quantity: reorder-quantity
        }))
        
        (ok true)
    )
)

;; Analytics and Performance Tracking
(define-public (calculate-demand-analytics (product-id uint))
    (let
        (
            (product (unwrap! (map-get? retail-products product-id) ERR_PRODUCT_NOT_FOUND))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (demand-volatility (get demand-variability product))
            (seasonal-index (get seasonality-factor product))
            (turnover-rate (if (> (get current-inventory product) u0)
                             (/ u365 (get lead-time-days product))
                             u0))
        )
        (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
        
        (map-set demand-analytics product-id {
            product-id: product-id,
            demand-volatility: demand-volatility,
            forecast-accuracy: u80,
            seasonal-index: seasonal-index,
            trend-coefficient: u100,
            demand-correlation: u75,
            stockout-frequency: u5,
            overstock-frequency: u10,
            turnover-rate: turnover-rate,
            fill-rate: u95,
            last-calculated: current-time
        })
        (ok true)
    )
)

;; Read-Only Functions
(define-read-only (get-product (product-id uint))
    (map-get? retail-products product-id)
)

(define-read-only (get-demand-forecast (forecast-id uint))
    (map-get? demand-forecasts forecast-id)
)

(define-read-only (get-sales-pattern (product-id uint) (period uint))
    (map-get? sales-patterns { product-id: product-id, period: period })
)

(define-read-only (get-seasonal-trend (trend-id uint))
    (map-get? seasonal-trends trend-id)
)

(define-read-only (get-inventory-recommendation (product-id uint))
    (map-get? inventory-recommendations product-id)
)

(define-read-only (get-demand-analytics (product-id uint))
    (map-get? demand-analytics product-id)
)

(define-read-only (get-planner-info (planner principal))
    (map-get? authorized-planners planner)
)

(define-read-only (get-contract-stats)
    (ok {
        total-products: (var-get total-products),
        total-forecasts: (var-get total-forecasts),
        next-product-id: (var-get next-product-id),
        next-forecast-id: (var-get next-forecast-id),
        demand-planning-enabled: (var-get demand-planning-enabled)
    })
)

(define-read-only (is-forecast-accurate (forecast-id uint))
    (let
        (
            (forecast (map-get? demand-forecasts forecast-id))
        )
        (match forecast
            forecast-data (ok (>= (get accuracy-score forecast-data) FORECAST_ACCURACY_THRESHOLD))
            (err ERR_FORECAST_NOT_FOUND)
        )
    )
)

