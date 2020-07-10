add_pfunit_ctest (libtt_common_testSuite
    TEST_SOURCES test_common_kronecker.pf test_common_trace.pf
    LINK_LIBRARIES tensortools
)

add_pfunit_ctest (libtt_physics_testSuite
    TEST_SOURCES test_physics_tensors.pf
    LINK_LIBRARIES tensortools
)

add_pfunit_ctest (libtt_products_testSuite
    TEST_SOURCES test_products_doubleContract.pf
    LINK_LIBRARIES tensortools
)