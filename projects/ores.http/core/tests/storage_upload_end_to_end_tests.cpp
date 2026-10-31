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
#include "ores.http.api/net/http_server.hpp"
#include "ores.http.core/routes/storage_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <memory>
#include <openssl/bio.h>
#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/rsa.h>
#include <string>
#include <thread>

using namespace ores::http::net;
using namespace ores::http::server::routes;
using namespace ores::logging;
using namespace ores::storage::filesystem;
namespace fs = std::filesystem;
namespace asio = boost::asio;

namespace {

/**
 * @brief An RSA key pair for the test's own tokens, made once.
 */
struct end_to_end_keys final {
    std::string private_key_pem;
    std::string public_key_pem;
};

end_to_end_keys make_end_to_end_keys() {
    end_to_end_keys result;

    auto* ctx = EVP_PKEY_CTX_new_id(EVP_PKEY_RSA, nullptr);
    REQUIRE(ctx != nullptr);
    REQUIRE(EVP_PKEY_keygen_init(ctx) == 1);
    REQUIRE(EVP_PKEY_CTX_set_rsa_keygen_bits(ctx, 2048) == 1);

    EVP_PKEY* key = nullptr;
    REQUIRE(EVP_PKEY_keygen(ctx, &key) == 1);
    EVP_PKEY_CTX_free(ctx);

    if (auto* bio = BIO_new(BIO_s_mem())) {
        PEM_write_bio_PrivateKey(bio, key, nullptr, nullptr, 0, nullptr, nullptr);
        char* data = nullptr;
        const auto length = BIO_get_mem_data(bio, &data);
        result.private_key_pem = std::string(data, static_cast<std::size_t>(length));
        BIO_free(bio);
    }
    if (auto* bio = BIO_new(BIO_s_mem())) {
        PEM_write_bio_PUBKEY(bio, key);
        char* data = nullptr;
        const auto length = BIO_get_mem_data(bio, &data);
        result.public_key_pem = std::string(data, static_cast<std::size_t>(length));
        BIO_free(bio);
    }

    EVP_PKEY_free(key);
    return result;
}

const end_to_end_keys& test_keys() {
    static const auto keys = make_end_to_end_keys();
    return keys;
}

/**
 * @brief A gateway serving the storage routes on a free port.
 */
class storage_gateway final {
public:
    storage_gateway(std::string storage_dir, std::uint64_t max_upload_bytes)
        : options_(make_options())
        , server_(std::make_shared<http_server>(io_, options_))
        , routes_(
              std::make_shared<storage_routes>(std::move(storage_dir), nullptr, max_upload_bytes)) {

        server_->set_verifier(std::make_shared<ores::security::jwt::jwt_authenticator>(
            ores::security::jwt::jwt_authenticator::create_rs256_verifier(
                test_keys().public_key_pem, std::string(issuer), std::string(audience))));
        routes_->register_routes(server_->get_router(), server_->get_registry());

        asio::co_spawn(
            io_, [this]() -> asio::awaitable<void> { co_await server_->run(); }, asio::detached);
        thread_ = std::thread([this] { io_.run(); });

        for (auto attempt = 0; attempt < 500 && port_ == 0; ++attempt) {
            port_ = server_->bound_port();
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }

    ~storage_gateway() {
        server_->stop();
        io_.stop();
        if (thread_.joinable())
            thread_.join();
    }

    storage_gateway(const storage_gateway&) = delete;
    storage_gateway& operator=(const storage_gateway&) = delete;

    [[nodiscard]] std::string url() const {
        return "http://127.0.0.1:" + std::to_string(port_);
    }

    static constexpr std::string_view issuer = "ores.test";
    static constexpr std::string_view audience = "ores.test";

private:
    static http_server_options make_options() {
        http_server_options options;
        options.address = "127.0.0.1";
        options.port = 0;
        options.worker_threads = 2;
        options.default_max_body_size = 1024 * 1024;
        options.max_body_size = 64ull * 1024 * 1024;
        return options;
    }

    asio::io_context io_;
    http_server_options options_;
    std::shared_ptr<http_server> server_;
    std::shared_ptr<storage_routes> routes_;
    std::thread thread_;
    std::uint16_t port_{0};
};

/**
 * @brief A token that names what it may touch, so no permission lookup runs.
 */
std::string capability_token(std::string_view bucket, std::string_view prefix) {
    auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
    claims.subject = "00000000-0000-0000-0000-000000000001";
    claims.issuer = std::string(storage_gateway::issuer);
    claims.audience = std::string(storage_gateway::audience);

    for (const auto* op : {"get", "put", "delete", "list"})
        claims.storage_grants.push_back(ores::security::jwt::storage_grant{
            .bucket = std::string(bucket), .key_prefix = std::string(prefix), .op = op});

    const auto signer = ores::security::jwt::jwt_authenticator::create_rs256_signer(
        test_keys().private_key_pem,
        std::string(storage_gateway::issuer),
        std::string(storage_gateway::audience));

    const auto token = signer.create_token(claims);
    REQUIRE(token.has_value());
    return *token;
}

/**
 * @brief A directory that removes itself, so a run leaves nothing behind.
 */
class scratch_dir final {
public:
    explicit scratch_dir(std::string_view name) {
        const auto stamp = std::chrono::steady_clock::now().time_since_epoch().count();
        path_ = fs::temp_directory_path() / (std::string(name) + std::to_string(stamp));
        fs::create_directories(path_);
    }

    ~scratch_dir() {
        std::error_code ec;
        fs::remove_all(path_, ec);
    }

    scratch_dir(const scratch_dir&) = delete;
    scratch_dir& operator=(const scratch_dir&) = delete;

    [[nodiscard]] const fs::path& path() const {
        return path_;
    }

private:
    fs::path path_;
};

/**
 * @brief Writes a file of the given size without holding it.
 */
fs::path write_file(const fs::path& path, std::uint64_t size, char filler) {
    std::ofstream out(path, std::ios::binary | std::ios::trunc);
    auto block = std::string(1024 * 1024, filler);
    for (std::uint64_t written = 0; written < size; written += block.size()) {
        const auto take = std::min<std::uint64_t>(block.size(), size - written);
        out.write(block.data(), static_cast<std::streamsize>(take));
    }
    out.close();
    return path;
}

}

TEST_CASE("an_upload_through_the_gateway_round_trips", "[storage_upload][end_to_end]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the upload end to end, through the real gateway";

    scratch_dir objects("ores-e2e-objects-");
    scratch_dir files("ores-e2e-files-");

    const auto size = 8ull * 1024 * 1024;
    const auto source = write_file(files.path() / "package.tar.gz", size, 'x');
    const auto expected_digest = ores::utility::crypto::sha256::hex_digest_of_file(source);

    storage_gateway gateway(objects.path().string(), 64ull * 1024 * 1024);
    ores::storage::net::storage_transfer client(gateway.url(), capability_token("ores", ""));

    const auto answer =
        client.upload_returning_response("ores", "compute/packages/package.tar.gz", source);

    // The object is at the key the caller named, and it is the file it sent.
    const local_store store(objects.path());
    REQUIRE(store.exists("ores", "compute/packages/package.tar.gz"));
    REQUIRE(store.size("ores", "compute/packages/package.tar.gz") == size);
    REQUIRE(ores::utility::crypto::sha256::hex_digest_of_file(
                *store.resolve("ores", "compute/packages/package.tar.gz")) == expected_digest);

    // The answer is the route's own, and it names the digest of what it stored.
    INFO("the gateway answered: " << answer);
    REQUIRE(answer.find(expected_digest) != std::string::npos);

    // No part file is left behind.
    auto parts = std::size_t{0};
    for (const auto& entry : fs::recursive_directory_iterator(objects.path()))
        if (entry.path().extension() == ".part")
            ++parts;
    REQUIRE(parts == 0);
}

TEST_CASE("a_download_through_the_gateway_returns_the_stored_bytes",
          "[storage_upload][end_to_end]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the round trip back through the gateway";

    scratch_dir objects("ores-e2e-objects-");
    scratch_dir files("ores-e2e-files-");

    const auto size = 3ull * 1024 * 1024;
    const auto source = write_file(files.path() / "package.tar.gz", size, 'y');

    storage_gateway gateway(objects.path().string(), 64ull * 1024 * 1024);
    ores::storage::net::storage_transfer client(gateway.url(), capability_token("ores", ""));

    client.upload("ores", "compute/packages/back.tar.gz", source);

    const auto fetched = files.path() / "fetched.tar.gz";
    client.download("ores", "compute/packages/back.tar.gz", fetched);

    REQUIRE(fs::file_size(fetched) == size);
    REQUIRE(ores::utility::crypto::sha256::hex_digest_of_file(fetched) ==
            ores::utility::crypto::sha256::hex_digest_of_file(source));
}

TEST_CASE("an_upload_outside_the_capability_is_refused_and_leaves_nothing",
          "[storage_upload][end_to_end]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a capability is the whole of what a caller may touch";

    scratch_dir objects("ores-e2e-objects-");
    scratch_dir files("ores-e2e-files-");

    const auto source = write_file(files.path() / "package.tar.gz", 1024 * 1024, 'z');

    storage_gateway gateway(objects.path().string(), 64ull * 1024 * 1024);
    // The token names another bucket, so this upload is outside its scope.
    ores::storage::net::storage_transfer client(gateway.url(), capability_token("elsewhere", ""));

    REQUIRE_THROWS(client.upload("ores", "compute/packages/package.tar.gz", source));

    const local_store store(objects.path());
    REQUIRE(!store.exists("ores", "compute/packages/package.tar.gz"));

    auto parts = std::size_t{0};
    if (fs::exists(objects.path()))
        for (const auto& entry : fs::recursive_directory_iterator(objects.path()))
            if (entry.path().extension() == ".part")
                ++parts;
    REQUIRE(parts == 0);
}
