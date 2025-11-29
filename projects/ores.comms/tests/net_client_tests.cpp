/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include <catch2/catch_test_macros.hpp>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <chrono>

#include "ores.comms/net/client.hpp"
#include "ores.comms/net/server.hpp"
#include "ores.comms/net/client_options.hpp"
#include "ores.comms/net/server_options.hpp"
#include "ores.testing/run_coroutine_test.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace {

const std::string test_suite("ores.comms.tests");
const std::string tags("[networking]");

// Embedded self-signed certificate and private key for testing
const std::string server_cert = R"(-----BEGIN CERTIFICATE-----
MIIDgTCCAmmgAwIBAgIUaV6QAAxLAN0vO97pugiqstoAZjswDQYJKoZIhvcNAQEL
BQAwaTELMAkGA1UEBhMCVVMxDjAMBgNVBAgMBVN0YXRlMQ0wCwYDVQQHDARDaXR5
MRUwEwYDVQQKDAxPcmdhbml6YXRpb24xEDAOBgNVBAsMB09yZ1VuaXQxEjAQBgNV
BAMMCWxvY2FsaG9zdDAeFw0yNTExMjkxNTI0NDVaFw0yNjExMjkxNTI0NDVaMGkx
CzAJBgNVBAYTAlVTMQ4wDAYDVQQIDAVTdGF0ZTENMAsGA1UEBwwEQ2l0eTEVMBMG
A1UECgwMT3JnYW5pemF0aW9uMRAwDgYDVQQLDAdPcmdVbml0MRIwEAYDVQQDDAls
b2NhbGhvc3QwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCWjY95EGBJ
cjEWcjVk6TD7UEV8UMUZP2d98ObvVgR2NsjyXpRUxey0gIfJQuXzwFp1FZtk2OFn
LeYHQVBIRO3yTrYUbG1uJAQW7z+BajeUl/jCjaD79XOxuUUEZHJhQL+LLS3nVKhZ
y8oxmYiC4//AtVzuBMiuBhjkEsLvX3BXMon+wE8tkUkOQyz2EnAMoBBQVixnlzyf
x6+r9l7RqM+Iqyatxb+ezl+pH4GtRJimkPt33qnkNg8JQjpZFGVbuSHL1NdmF9t4
wAbjFoq5ROkFM1V01JBdgBKABROKu7YMhbFt6iMs5vwBCPBSYjCp0EXL57CUs3w+
rJuHZ2BcNdY7AgMBAAGjITAfMB0GA1UdDgQWBBQu0LVFYvFPM/bXKCfK4wl4PhfV
uzANBgkqhkiG9w0BAQsFAAOCAQEABu/xP4ykh2lcdB2s1VxIylbbSRm7VGA1fAg9
88mCg+cCj7VcbQk/H60vBUgrKoOIPN/9p/U71lj7fzITyqBtGZdLjX3A3QGspvKQ
7ZKgbNv19tD4ik0hkkzTdB7KJJJt1EfjyXWMk5xe5TJLbsrKRYMCQigRb2IJuIel
4XqLlC7Aw0Wsk0kf+BbOOH+xOEkItOWfo9+J6itswnzxYhkTwKGW4KBDC4i69Qdv
NLmox+OuIYtggtkOFTvQA+LiQtYvj3ROqW3O5gtmNIkufchLjSmsFOcxeTXW5y8w
uwcheLZ1kyxYXJJ0jszJBl3FdZfhn5JyXwOsmw0jYst+0dYAhQ==
-----END CERTIFICATE-----
)";

const std::string server_key = R"(-----BEGIN PRIVATE KEY-----
MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCWjY95EGBJcjEW
cjVk6TD7UEV8UMUZP2d98ObvVgR2NsjyXpRUxey0gIfJQuXzwFp1FZtk2OFnLeYH
QVBIRO3yTrYUbG1uJAQW7z+BajeUl/jCjaD79XOxuUUEZHJhQL+LLS3nVKhZy8ox
mYiC4//AtVzuBMiuBhjkEsLvX3BXMon+wE8tkUkOQyz2EnAMoBBQVixnlzyfx6+r
9l7RqM+Iqyatxb+ezl+pH4GtRJimkPt33qnkNg8JQjpZFGVbuSHL1NdmF9t4wAbj
Foq5ROkFM1V01JBdgBKABROKu7YMhbFt6iMs5vwBCPBSYjCp0EXL57CUs3w+rJuH
Z2BcNdY7AgMBAAECggEAMfRHqkpqQISA0CtWFyUe6QyB+dVNCOsWWl4AKmLWuSjF
dVI3kodX2q6ZH4kwhrgI67efJ4+k/tjmbfrYehpQ/6jpVo6Bot2Vc8yvvC+zwHzK
gDeBNokDefF4+pOftjBHPhVgGVVoRN9PODTy7EdduaIT8zAoLEH+qVKmVsnKm0u5
heYTs0pKuMcqNz1wQK1JYFBvVJihN8+nkxOW8Hq7pyS92MVfDYHULlM6GQrwE8QH
0y4hnWGtGiOCTT0KuRoXtzeNTxgaH/AcrGfN15NDaab1oUGVJ+xySvd30wzfEAGv
dFAjq/2zzTXyrhJnXbYMrMFBc3PZa5jGrmjKT8W13QKBgQDTXbGu6f34rvpvGqrW
vu9j9HsBoAcnsgxhptzEwQTqkO9dlCXJmwlmqop8cn1gg7o4w5dENOpyzjXtUv6x
GIUoPlvKF/5bOmu9LC3g1KHC8kNdGul2wXW8ya59u6WNaezTxcwSZ3RQP7YIX4Dt
dWS8CZbckAInPB1SmMztYfpOnQKBgQC2WFrFDIJqEKIQ4NF4L5lZ6hml8Y/WMwU/
31rzeIfr2CYmkW/6aEXoxqVbEmXe4tAID0mAQtldHsTiJk7fQpNLHXc1kkUFgJyf
gboDElTKb/u1vVdHbyH9PJbdo5EoMfLVBwbSH0MfwEpy4dyZ8AvMmBgkFLwzGw5a
wmdTAKL0twKBgHbVYIaEy+M7DVXV2mLt6k16MIYTYLJyqf9k/w4nrwnp/onQNwaI
AdsU2tNo0xCmPtaXML6KOZ3eTfdmCAetT3/2v0h6AOltiRFMe2bCZUfjSPhZnxTD
Y9yGT3IlQ8ij2yCdPkTCoWRUzZUbjLL0o3QMuCK9XFiOtbWwbtavlTkJAoGAWdPK
23mfIVsSS5hE+J0SQY4mlm4aup2lv2Jrrrc41kFEAgnxjP0jO1noiaXVk2hOrTnC
rrG/3LTUrsfKYQeuAfoBCN4GrIBAJ//DP9Ie/R8Pc+dTgLhJmTPo6kSytIdupezx
Oj/2VETpKBEGGLt9RIvEkLm6QL7aYnGUbvpITJ8CgYEArYm9DwqezE/R6Ri4vYiC
LCXP9kUyGh8kETsXiu5o2uHS7xohhv3X4VP/jNvt0d0bm1N7WIG64ro3CMkMlkTO
bTtEBJ7zhzzcg8vnLIaxgTwdrqN0MYzNCEqJFXljRtV+J+A/+oOJlclq6lHXd0nY
tSF6X2Tz8FU6Whed2zL17v8=
-----END PRIVATE KEY-----
)";



}

TEST_CASE("test_client_server_connection", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Starting test_client_server_connection";
    boost::asio::io_context io_context;

    ores::testing::run_coroutine_test(io_context, [&]() -> boost::asio::awaitable<void> {
        BOOST_LOG_SEV(lg, info) << "Inside test coroutine";
        // Configure server with port 0 (dynamic) and in-memory certs
        ores::comms::net::server_options server_opts;
        server_opts.port = 0;
        server_opts.certificate_chain_content = server_cert;
        server_opts.private_key_content = server_key;
        server_opts.enable_signal_watching = false;

        BOOST_LOG_SEV(lg, info) << "Creating server";
        // Create server
        auto server = std::make_shared<ores::comms::net::server>(server_opts);

        std::uint16_t server_port = 0;

        BOOST_LOG_SEV(lg, info) << "Spawning server";
        // Start server in background with callback
        boost::asio::co_spawn(co_await boost::asio::this_coro::executor,
            [server, &io_context, &server_port]() -> boost::asio::awaitable<void> {
                auto lg = ores::utility::log::make_logger("ores.comms.tests.server_runner");
                BOOST_LOG_SEV(lg, info) << "Server starting run loop";
                co_await server->run(io_context, [&](std::uint16_t port) {
                    server_port = port;
                    BOOST_LOG_SEV(lg, info) << "Server listening on port: " << port;
                });
                BOOST_LOG_SEV(lg, info) << "Server run loop finished";
            }, boost::asio::detached);

        BOOST_LOG_SEV(lg, info) << "Waiting for server port assignment";
        // Wait for server to start and assign port
        boost::asio::steady_timer timer(co_await boost::asio::this_coro::executor);
        while (server_port == 0) {
            timer.expires_after(std::chrono::milliseconds(10));
            co_await timer.async_wait(boost::asio::use_awaitable);
        }
        BOOST_LOG_SEV(lg, info) << "Server port assigned: " << server_port;

        // Configure client with assigned port
        ores::comms::net::client_options client_opts;
        client_opts.host = "localhost";
        client_opts.port = server_port;
        client_opts.verify_certificate = false; // Self-signed cert

        BOOST_LOG_SEV(lg, info) << "Creating client";
        // Create client
        auto client = std::make_shared<ores::comms::net::client>(client_opts, co_await boost::asio::this_coro::executor);

        BOOST_LOG_SEV(lg, info) << "Connecting client";
        // Connect client
        co_await client->connect();

        BOOST_LOG_SEV(lg, info) << "Client connected";
        CHECK(client->is_connected());

        // Clean disconnect
        client->disconnect();
        CHECK(!client->is_connected());
        
        BOOST_LOG_SEV(lg, info) << "Stopping server";
        server->stop();
        BOOST_LOG_SEV(lg, info) << "Test finished";
    });
}
