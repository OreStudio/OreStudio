/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.http.client/net/transport.hpp"
#include "ores.platform/process/pid.hpp"
#include <boost/asio/buffer.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ssl/context.hpp>
#include <boost/asio/ssl/stream.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/beast/core/flat_buffer.hpp>
#include <boost/beast/core/stream_traits.hpp>
#include <boost/beast/http/read.hpp>
#include <boost/beast/http/string_body.hpp>
#include <boost/beast/http/write.hpp>
#include <catch2/catch_test_case_info.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/reporters/catch_reporter_event_listener.hpp>
#include <catch2/reporters/catch_reporter_registrars.hpp>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <memory>
#include <openssl/pem.h>
#include <openssl/x509v3.h>
#include <optional>
#include <string>
#include <thread>

namespace {

namespace asio = boost::asio;
namespace bhttp = boost::beast::http;
using tcp = asio::ip::tcp;
using namespace ores::http::client::net;
using namespace std::chrono_literals;

struct certificate {
    std::string cert_pem;
    std::string key_pem;
};

std::string pem_of(const std::function<int(BIO*)>& write) {
    std::unique_ptr<BIO, decltype(&BIO_free)> bio(BIO_new(BIO_s_mem()), BIO_free);
    REQUIRE(write(bio.get()) == 1);
    char* data = nullptr;
    const auto size = BIO_get_mem_data(bio.get(), &data);
    return std::string(data, static_cast<std::size_t>(size));
}

/// A self-signed certificate for @p host, made for one test and trusted only where a test
/// says so.
certificate make_certificate(const std::string& host) {
    std::unique_ptr<EVP_PKEY, decltype(&EVP_PKEY_free)> key(EVP_RSA_gen(2048), EVP_PKEY_free);
    REQUIRE(key);
    std::unique_ptr<X509, decltype(&X509_free)> x(X509_new(), X509_free);
    X509_set_version(x.get(), 2);
    ASN1_INTEGER_set(X509_get_serialNumber(x.get()), 1);
    X509_gmtime_adj(X509_getm_notBefore(x.get()), -60);
    X509_gmtime_adj(X509_getm_notAfter(x.get()), 3600);
    X509_set_pubkey(x.get(), key.get());
    auto* name = X509_get_subject_name(x.get());
    X509_NAME_add_entry_by_txt(
        name, "CN", MBSTRING_ASC, reinterpret_cast<const unsigned char*>(host.c_str()), -1, -1, 0);
    X509_set_issuer_name(x.get(), name);
    X509V3_CTX ctx;
    X509V3_set_ctx_nodb(&ctx);
    X509V3_set_ctx(&ctx, x.get(), x.get(), nullptr, nullptr, 0);
    for (const auto& [nid, value] : {std::pair{NID_subject_alt_name, "DNS:" + host},
                                     std::pair{NID_basic_constraints, std::string("CA:TRUE")}}) {
        auto* ext = X509V3_EXT_conf_nid(nullptr, &ctx, nid, value.c_str());
        REQUIRE(ext);
        X509_add_ext(x.get(), ext, -1);
        X509_EXTENSION_free(ext);
    }
    REQUIRE(X509_sign(x.get(), key.get(), EVP_sha256()) > 0);
    return {pem_of([&](BIO* b) { return PEM_write_bio_X509(b, x.get()); }), pem_of([&](BIO* b) {
                return PEM_write_bio_PrivateKey(
                    b, key.get(), nullptr, nullptr, 0, nullptr, nullptr);
            })};
}

std::filesystem::path written(const std::string& pem) {
    static int n = 0;
    const auto path = std::filesystem::temp_directory_path() /
                      ("ores_http_client_test_" + std::to_string(ores::platform::process::current_pid()) + "_" +
                       std::to_string(n++) + ".pem");
    std::ofstream(path) << pem;
    return path;
}

/// What the local server does with the one request it takes.
struct reply {
    std::string body = "hello";
    std::chrono::milliseconds delay{0};
};

/// One connection to the local server: reads a request, waits, then answers it.
template <typename Stream>
class session final : public std::enable_shared_from_this<session<Stream>> {
public:
    session(Stream stream, reply r)
        : stream_(std::move(stream)), timer_(stream_.get_executor()), reply_(std::move(r)) {}

    void serve() {
        bhttp::async_read(stream_, buffer_, request_,
                          [self = this->shared_from_this()](boost::system::error_code ec,
                                                            std::size_t) {
                              if (!ec)
                                  self->answer_later();
                          });
    }

    Stream& stream() {
        return stream_;
    }

private:
    void answer_later() {
        timer_.expires_after(reply_.delay);
        timer_.async_wait([self = this->shared_from_this()](boost::system::error_code ec) {
            if (!ec)
                self->answer();
        });
    }

    void answer() {
        response_.set(bhttp::field::content_type, "application/json");
        response_.body() = reply_.body;
        response_.prepare_payload();
        bhttp::async_write(
            stream_, response_,
            [self = this->shared_from_this()](boost::system::error_code, std::size_t) {
                boost::system::error_code ignored;
                boost::beast::get_lowest_layer(self->stream_).close(ignored);
            });
    }

    Stream stream_;
    asio::steady_timer timer_;
    reply reply_;
    boost::beast::flat_buffer buffer_;
    bhttp::request<bhttp::string_body> request_;
    bhttp::response<bhttp::string_body> response_{bhttp::status::ok, 11};
};

/**
 * @brief A server on a free local port that answers one connection, over TLS when
 * given a certificate.
 *
 * Every step is asynchronous on the server's own context, and the destructor stops
 * that context. No accept, handshake or read can then keep the test waiting, whatever
 * the client did or did not send.
 */
class local_server {
public:
    explicit local_server(reply r,
                          std::optional<certificate> cert = std::nullopt,
                          const std::string& address = "127.0.0.1")
        : acceptor_(io_, tcp::endpoint(asio::ip::make_address(address), 0)), reply_(std::move(r)) {
        port_ = acceptor_.local_endpoint().port();
        if (cert) {
            tls_.emplace(asio::ssl::context::tls_server);
            tls_->use_certificate_chain(asio::buffer(cert->cert_pem));
            tls_->use_private_key(asio::buffer(cert->key_pem), asio::ssl::context::pem);
        }
        acceptor_.async_accept([this](boost::system::error_code ec, tcp::socket socket) {
            if (!ec)
                accepted(std::move(socket));
        });
        thread_ = std::thread([this] { io_.run(); });
    }

    ~local_server() {
        io_.stop();
        if (thread_.joinable())
            thread_.join();
    }

    local_server(const local_server&) = delete;
    local_server& operator=(const local_server&) = delete;

    [[nodiscard]] std::string url(const std::string& scheme, const std::string& host) const {
        return scheme + "://" + host + ":" + std::to_string(port_) + "/v2/rates?from=2026-10-01";
    }

private:
    void accepted(tcp::socket socket) {
        if (!tls_) {
            std::make_shared<session<tcp::socket>>(std::move(socket), reply_)->serve();
            return;
        }
        using tls_stream = asio::ssl::stream<tcp::socket>;
        auto s = std::make_shared<session<tls_stream>>(tls_stream(std::move(socket), *tls_), reply_);
        s->stream().async_handshake(asio::ssl::stream_base::server,
                                    [s](boost::system::error_code ec) {
                                        if (!ec)
                                            s->serve();
                                    });
    }

    asio::io_context io_;
    std::optional<asio::ssl::context> tls_;
    tcp::acceptor acceptor_;
    reply reply_;
    unsigned short port_ = 0;
    std::thread thread_;
};

}

namespace {

// The suite reports a test only when it ends, so a test that hangs until the
// CTest timeout would otherwise leave no name behind.
class test_start_reporter final : public Catch::EventListenerBase {
public:
    using Catch::EventListenerBase::EventListenerBase;

    void testCaseStarting(const Catch::TestCaseInfo& info) override {
        std::cerr << "starting: " << info.name << std::endl;
    }
};

}

CATCH_REGISTER_LISTENER(test_start_reporter)

TEST_CASE("a_get_over_http_returns_the_status_type_and_body", "[net][transport]") {
    local_server server({.body = R"({"rates":[]})"});
    beast_transport sut;

    const auto out = sut.get(server.url("http", "127.0.0.1"));

    REQUIRE(out.has_value());
    CHECK(out->status == 200);
    CHECK(out->content_type == "application/json");
    CHECK(out->body == R"({"rates":[]})");
}

TEST_CASE("a_body_over_the_limit_is_refused", "[net][transport]") {
    local_server server({.body = std::string(2000, 'x')});
    beast_transport sut;

    const auto out = sut.get(server.url("http", "127.0.0.1"), {.max_body_bytes = 100});

    REQUIRE_FALSE(out.has_value());
    CHECK(out.error().kind == failure_kind::too_large);
}

TEST_CASE("a_server_that_answers_too_late_times_out", "[net][transport]") {
    local_server server({.delay = 1500ms});
    beast_transport sut;

    const auto started = std::chrono::steady_clock::now();
    const auto out = sut.get(server.url("http", "127.0.0.1"), {.timeout = 200ms});

    REQUIRE_FALSE(out.has_value());
    CHECK(out.error().kind == failure_kind::timeout);
    CHECK(std::chrono::steady_clock::now() - started < 1000ms);
}

TEST_CASE("a_get_over_https_to_a_trusted_certificate_for_its_host_succeeds", "[net][transport]") {
    const auto cert = make_certificate("localhost");
    local_server server({.body = "secure"}, cert);
    beast_transport sut({.trusted_authorities = written(cert.cert_pem)});

    const auto out = sut.get(server.url("https", "localhost"));

    REQUIRE(out.has_value());
    CHECK(out->body == "secure");
}

TEST_CASE("a_certificate_that_no_trusted_authority_signed_is_refused", "[net][transport]") {
    local_server server({}, make_certificate("localhost"));
    beast_transport sut;

    const auto out = sut.get(server.url("https", "localhost"));

    REQUIRE_FALSE(out.has_value());
    CHECK(out.error().kind == failure_kind::tls);
}

TEST_CASE("a_trusted_certificate_for_another_host_is_refused", "[net][transport]") {
    const auto cert = make_certificate("other.example");
    local_server server({}, cert);
    beast_transport sut({.trusted_authorities = written(cert.cert_pem)});

    const auto out = sut.get(server.url("https", "localhost"));

    REQUIRE_FALSE(out.has_value());
    CHECK(out.error().kind == failure_kind::tls);
}

TEST_CASE("an_address_that_is_not_http_or_https_is_refused", "[net][transport]") {
    beast_transport sut;

    CHECK(sut.get("ftp://example.org/x").error().kind == failure_kind::bad_url);
    CHECK(sut.get("not an address").error().kind == failure_kind::bad_url);
}

TEST_CASE("an_ipv6_literal_host_is_read", "[net][transport]") {
    std::unique_ptr<local_server> server;
    try {
        server = std::make_unique<local_server>(reply{.body = "six"}, std::nullopt, "::1");
    } catch (const std::exception&) {
        SKIP("This host has no IPv6 loopback");
    }
    beast_transport sut;

    const auto out = sut.get(server->url("http", "[::1]"));

    REQUIRE(out.has_value());
    CHECK(out->body == "six");
}

TEST_CASE("the_user_and_password_of_an_address_are_masked", "[net][transport]") {
    CHECK(redacted("https://alice:s3cret@api.example.org/series?from=2026-10-01") ==
          "https://***:***@api.example.org/series?from=2026-10-01");
    CHECK(redacted("https://alice@api.example.org/series") == "https://***@api.example.org/series");
}

TEST_CASE("a_secret_in_the_query_is_masked_and_the_rest_kept", "[net][transport]") {
    CHECK(redacted("https://api.example.org/series?api_key=abc123&from=2026-10-01") ==
          "https://api.example.org/series?api_key=***&from=2026-10-01");
    CHECK(redacted("https://api.example.org/series?Token=x&series_id=DEXUSEU") ==
          "https://api.example.org/series?Token=***&series_id=DEXUSEU");
    CHECK(redacted("https://api.example.org/series?from=2026-10-01") ==
          "https://api.example.org/series?from=2026-10-01");
}

TEST_CASE("a_failure_names_the_address_with_its_secret_masked", "[net][transport]") {
    beast_transport sut;

    const auto out = sut.get("http://127.0.0.1:1/x?api_key=abc123", {.timeout = 500ms});

    REQUIRE_FALSE(out.has_value());
    CHECK(out.error().message.contains("api_key=***"));
    CHECK_FALSE(out.error().message.contains("abc123"));
}

TEST_CASE("the_hosted_frankfurter_answers_over_the_system_trust_store",
          "[.][live][net][transport]") {
    beast_transport sut;

    const auto out = sut.get("https://api.frankfurter.dev/v2/providers", {.timeout = 15s});

    REQUIRE(out.has_value());
    CHECK(out->status == 200);
    CHECK(out->body.contains("\"ECB\""));
}
