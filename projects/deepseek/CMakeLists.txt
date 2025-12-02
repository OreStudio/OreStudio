# Deepseek Boost.Asio cancellation example test
add_executable(deepseek.test
    main.cpp
)

target_link_libraries(deepseek.test PRIVATE
    Boost::system
    Boost::thread
    OpenSSL::SSL
    OpenSSL::Crypto
)

target_compile_features(deepseek.test PRIVATE cxx_std_23)

install(
    TARGETS deepseek.test
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
