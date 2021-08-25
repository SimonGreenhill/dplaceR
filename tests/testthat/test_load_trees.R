library(dplaceR)


test_that("test load_trees error invalid type", {
    expect_error(
        load_trees("testdata/glottolog_utoa1244/", 'xx'),
        "Unknown Tree Type"
    )
})


test_that("test load_trees error invalid file", {
    expect_error(
        load_trees("testdata/glottolog_X"),
        "Invalid file"
    )
})


test_that("test load_trees error invalid renameto", {
    expect_error(
        load_trees("testdata/glottolog_utoa1244", renameto='xx'),
        "colname xx not in"
    )
})


test_that("test load_trees - glottolog", {
    trees <- load_trees("testdata/glottolog_utoa1244/")
    expect_equal(class(trees), 'multiPhylo')
    expect_equal(length(trees), 1)
    expect_equal(trees[[1]]$tip.label[[1]], 'cahu1264')
})


test_that("test load_trees - kolipakam - summary", {
    trees <- load_trees("testdata/kolipakam_et_al2018/")
    expect_equal(class(trees), 'multiPhylo')
    expect_equal(length(trees), 1)
    expect_equal(trees[[1]]$tip.label[[1]], 'Badga')
})


test_that("test load_trees - kolipakam - posterior", {
    trees <- load_trees("testdata/kolipakam_et_al2018/", type="posterior")
    expect_equal(class(trees), 'multiPhylo')
    expect_equal(length(trees), 2)
    expect_equal(trees[[1]]$tip.label[[1]], 'Malto')
})


test_that("test load_trees - kolipakam - remapping", {
    trees <- load_trees("testdata/kolipakam_et_al2018/", type="posterior", renameto='glottocode')
    expect_equal(class(trees), 'multiPhylo')
    expect_equal(length(trees), 2)

    expected <- c("saur1249", "kuru1302", "brah1256", "koya1251")
    expect_equal(trees[[1]]$tip.label[1:4], expected)

    # xd_ids column
    trees <- load_trees("testdata/kolipakam_et_al2018/", type="posterior", renameto='xd_ids')
    expect_equal(class(trees), 'multiPhylo')
    expect_equal(length(trees), 2)

    expected <- c("xd668", "xd674")
    expect_equal(length(trees[[1]]$tip.label), 8)
    expect_equal(trees[[1]]$tip.label[1:2], expected)

})


test_that("test load_trees - duplicates in rename", {
    expect_warning(
        trees <- load_trees("testdata/glottolog_utoa1244_with_duplicates/", renameto="glottocode"),
        "Removing  3 tips that will be duplicated after rename: bann1248, cent2131, chem1251"
    )
    expect_equal(length(trees[[1]]$tip.label), 3)
    expect_equal(trees[[1]]$tip.label, c('B', 'C', 'A'))  # order in tree newick
})
