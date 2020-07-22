add_pfunit_ctest (libtt_common_testSuite
    TEST_SOURCES test_common.pf
    LINK_LIBRARIES tensortools
)

add_pfunit_ctest (libtt_tensors_common_testSuite
    TEST_SOURCES test_tensors_common.pf
    LINK_LIBRARIES tensortools
)

add_pfunit_ctest (libtt_tensors_physics_testSuite
    TEST_SOURCES test_tensors_physics.pf
    LINK_LIBRARIES tensortools
)

add_pfunit_ctest (libtt_products_testSuite
    TEST_SOURCES test_products_doubleContract.pf test_products_dyadic.pf
    LINK_LIBRARIES tensortools
)

add_pfunit_ctest (libtt_voigt_testSuite
    TEST_SOURCES test_voigt.pf
    LINK_LIBRARIES tensortools
)

add_pfunit_ctest (libtt_solvers_testSuite
    TEST_SOURCES test_solvers.pf
    LINK_LIBRARIES tensortools
)