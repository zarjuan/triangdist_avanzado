test_that("dtriang funciona en casos básicos", {
  expect_equal(
    dtriang(0.5, min = 0, max = 1, mode = 0.5),
    2 * (0.5 - 0) / ((1 - 0) * (0.5 - 0))
  )

  expect_equal(dtriang(-1, 0, 1, 0.5), 0)
  expect_equal(dtriang(2, 0, 1, 0.5), 0)
})

test_that("ptriang y qtriang son inversas", {
  p <- c(0.1, 0.5, 0.9)
  x <- qtriang(p, min = 0, max = 1, mode = 0.5)
  expect_equal(
    ptriang(x, min = 0, max = 1, mode = 0.5),
    p,
    tolerance = 1e-8
  )
})

test_that("rtriang genera valores dentro del soporte", {
  set.seed(123)
  x <- rtriang(1000, min = 0, max = 1, mode = 0.5)
  expect_true(all(x >= 0 & x <= 1))
})

test_that("errores de parámetros", {
  expect_error(dtriang(0.5, min = 1, max = 0, mode = 0.5))
  expect_error(dtriang(0.5, min = 0, max = 1, mode = 2))
  expect_error(qtriang(-0.1, min = 0, max = 1, mode = 0.5))
  expect_error(rtriang(-5, min = 0, max = 1, mode = 0.5))
})

test_that("casos extremos adicionales", {
  # ptriang: valores exactamente en los bordes
  expect_equal(ptriang(0, min = 0, max = 1, mode = 0.5), 0)
  expect_equal(ptriang(1, min = 0, max = 1, mode = 0.5), 1)

  # qtriang: probabilidades exactamente en los bordes
  expect_equal(qtriang(0, min = 0, max = 1, mode = 0.5), 0)
  expect_equal(qtriang(1, min = 0, max = 1, mode = 0.5), 1)
})

test_that("ptriang cubre ramas de valores intermedios", {
  # Punto exacto de la moda
  expect_equal(
    ptriang(0.5, min = 0, max = 1, mode = 0.5),
    0.5
  )
})

test_that("qtriang cubre ramas intermedias", {
  # Probabilidad exactamente igual a Fc
  Fc <- (0.5 - 0) / (1 - 0)
  expect_equal(
    qtriang(Fc, min = 0, max = 1, mode = 0.5),
    0.5
  )
})

test_that("dtriang cubre ramas exactas", {
  # Punto exacto de la moda
  expect_equal(
    dtriang(0.5, min = 0, max = 1, mode = 0.5),
    2
  )
})

test_that("rtriang cubre llamada interna a qtriang", {
  set.seed(1)
  x <- rtriang(1, min = 0, max = 1, mode = 0.5)
  expect_true(x >= 0 && x <= 1)
})
