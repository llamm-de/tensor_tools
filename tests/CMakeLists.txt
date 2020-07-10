add_pfunit_ctest (libtt_common_testSuite
    TEST_SOURCES test_common_kronecker.pf test_common_trace.pf
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
    TEST_SOURCES test_products_doubleContract.pf
    LINK_LIBRARIES tensortools
)