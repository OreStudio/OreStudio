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
#include "ores.security/jwt/jwt_authenticator.hpp"

#include <string>
#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/rsa.h>
#include <openssl/bio.h>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.security.tests");
const std::string tags("[jwt_authenticator]");
const std::string test_secret("super-secret-key-for-testing-purposes-only-32bytes!");
const std::string test_issuer("test-issuer");
const std::string test_audience("test-audience");

struct rsa_keypair {
    std::string private_key_pem;
    std::string public_key_pem;
};

/**
 * @brief Generates a 2048-bit RSA keypair for testing using OpenSSL.
 */
rsa_keypair generate_rsa_keypair() {
    rsa_keypair result;

    EVP_PKEY_CTX* ctx = EVP_PKEY_CTX_new_id(EVP_PKEY_RSA, nullptr);
    EVP_PKEY_keygen_init(ctx);
    EVP_PKEY_CTX_set_rsa_keygen_bits(ctx, 2048);

    EVP_PKEY* pkey = nullptr;
    EVP_PKEY_keygen(ctx, &pkey);
    EVP_PKEY_CTX_free(ctx);

    // Write private key
    BIO* bio = BIO_new(BIO_s_mem());
    PEM_write_bio_PrivateKey(bio, pkey, nullptr, nullptr, 0, nullptr, nullptr);
    BUF_MEM* bptr = nullptr;
    BIO_get_mem_ptr(bio, &bptr);
    result.private_key_pem = std::string(bptr->data, bptr->length);
    BIO_free(bio);

    // Write public key
    bio = BIO_new(BIO_s_mem());
    PEM_write_bio_PUBKEY(bio, pkey);
    BIO_get_mem_ptr(bio, &bptr);
    result.public_key_pem = std::string(bptr->data, bptr->length);
    BIO_free(bio);

    EVP_PKEY_free(pkey);
    return result;
}

// Generate once for the test suite (2048-bit generation is slow).
const rsa_keypair& test_rsa_keys() {
    static rsa_keypair keys = generate_rsa_keypair();
    return keys;
}

}

using namespace ores::security::jwt;
using namespace ores::logging;

// ---------------------------------------------------------------------------
// HS256 tests
// ---------------------------------------------------------------------------

TEST_CASE("jwt_authenticator_create_hs256_configured", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing HS256 authenticator creation";

    auto auth = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);

    REQUIRE(auth.is_configured());
}

TEST_CASE("jwt_authenticator_create_hs256_empty_secret_not_configured", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing HS256 with empty secret";

    auto auth = jwt_authenticator::create_hs256("", test_issuer, test_audience);

    REQUIRE_FALSE(auth.is_configured());
}

TEST_CASE("jwt_authenticator_create_and_validate_hs256_token", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing HS256 token creation and validation";

    auto auth = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);

    jwt_claims claims;
    claims.subject = "user123";
    claims.issuer = test_issuer;
    claims.audience = test_audience;
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = std::chrono::system_clock::now() + std::chrono::hours(1);
    claims.roles = {"admin", "user"};
    claims.username = "testuser";
    claims.email = "test@example.com";

    auto token = auth.create_token(claims);
    REQUIRE(token.has_value());
    REQUIRE_FALSE(token->empty());

    auto validated = auth.validate(*token);
    REQUIRE(validated.has_value());
    REQUIRE(validated->subject == "user123");
    REQUIRE(validated->roles.size() == 2);
    REQUIRE(validated->roles[0] == "admin");
    REQUIRE(validated->roles[1] == "user");
    REQUIRE(validated->username.has_value());
    REQUIRE(*validated->username == "testuser");
    REQUIRE(validated->email.has_value());
    REQUIRE(*validated->email == "test@example.com");
}

TEST_CASE("jwt_authenticator_validate_expired_token", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing expired token validation";

    auto auth = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);

    jwt_claims claims;
    claims.subject = "user123";
    claims.issued_at = std::chrono::system_clock::now() - std::chrono::hours(2);
    claims.expires_at = std::chrono::system_clock::now() - std::chrono::hours(1);

    auto token = auth.create_token(claims);
    REQUIRE(token.has_value());

    auto validated = auth.validate(*token);
    REQUIRE_FALSE(validated.has_value());
    REQUIRE(validated.error() == jwt_error::expired_token);
}

TEST_CASE("jwt_authenticator_validate_invalid_hs256_signature", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing invalid HS256 signature detection";

    auto auth1 = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);
    auto auth2 = jwt_authenticator::create_hs256("different-secret-key-32-bytes!!", test_issuer, test_audience);

    jwt_claims claims;
    claims.subject = "user123";
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = std::chrono::system_clock::now() + std::chrono::hours(1);

    auto token = auth1.create_token(claims);
    REQUIRE(token.has_value());

    auto validated = auth2.validate(*token);
    REQUIRE_FALSE(validated.has_value());
    REQUIRE((validated.error() == jwt_error::invalid_signature ||
             validated.error() == jwt_error::invalid_token));
}

TEST_CASE("jwt_authenticator_validate_malformed_token", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing malformed token detection";

    auto auth = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);

    auto validated = auth.validate("not.a.valid.jwt.token");
    REQUIRE_FALSE(validated.has_value());
    REQUIRE(validated.error() == jwt_error::invalid_token);
}

TEST_CASE("jwt_authenticator_unconfigured_validate_fails", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing unconfigured authenticator validation";

    auto auth = jwt_authenticator::create_hs256("", test_issuer, test_audience);

    auto validated = auth.validate("any.token.here");
    REQUIRE_FALSE(validated.has_value());
    REQUIRE(validated.error() == jwt_error::invalid_token);
}

// ---------------------------------------------------------------------------
// RS256 tests
// ---------------------------------------------------------------------------

TEST_CASE("jwt_authenticator_create_rs256_verifier_configured", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing RS256 verifier creation";

    auto auth = jwt_authenticator::create_rs256_verifier(
        test_rsa_keys().public_key_pem, test_issuer, test_audience);

    REQUIRE(auth.is_configured());
}

TEST_CASE("jwt_authenticator_create_rs256_verifier_empty_key_not_configured", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing RS256 verifier with empty key";

    auto auth = jwt_authenticator::create_rs256_verifier("", test_issuer, test_audience);

    REQUIRE_FALSE(auth.is_configured());
}

TEST_CASE("jwt_authenticator_rs256_verifier_cannot_create_token", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing RS256 verifier cannot create tokens";

    auto verifier = jwt_authenticator::create_rs256_verifier(
        test_rsa_keys().public_key_pem, test_issuer, test_audience);

    jwt_claims claims;
    claims.subject = "user123";
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = std::chrono::system_clock::now() + std::chrono::hours(1);

    auto token = verifier.create_token(claims);
    REQUIRE_FALSE(token.has_value());
}

TEST_CASE("jwt_authenticator_rs256_round_trip", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing RS256 sign/verify round-trip";

    const auto& keys = test_rsa_keys();
    auto signer = jwt_authenticator::create_rs256_signer(
        keys.private_key_pem, test_issuer, test_audience);
    auto verifier = jwt_authenticator::create_rs256_verifier(
        keys.public_key_pem, test_issuer, test_audience);

    jwt_claims claims;
    claims.subject = "account-uuid-123";
    claims.issuer = test_issuer;
    claims.audience = test_audience;
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = std::chrono::system_clock::now() + std::chrono::hours(1);
    claims.roles = {"trader", "viewer"};
    claims.session_id = "session-uuid-456";
    claims.tenant_id = "tenant-uuid-789";
    claims.party_id = "party-uuid-000";

    auto token = signer.create_token(claims);
    REQUIRE(token.has_value());
    REQUIRE_FALSE(token->empty());

    auto validated = verifier.validate(*token);
    REQUIRE(validated.has_value());
    REQUIRE(validated->subject == "account-uuid-123");
    REQUIRE(validated->roles.size() == 2);
    REQUIRE(validated->roles[0] == "trader");
    REQUIRE(validated->session_id.has_value());
    REQUIRE(*validated->session_id == "session-uuid-456");
    REQUIRE(validated->tenant_id.has_value());
    REQUIRE(*validated->tenant_id == "tenant-uuid-789");
    REQUIRE(validated->party_id.has_value());
    REQUIRE(*validated->party_id == "party-uuid-000");
}

TEST_CASE("jwt_authenticator_rs256_tamper_rejected", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing RS256 tampered token rejected";

    const auto& keys = test_rsa_keys();
    auto signer = jwt_authenticator::create_rs256_signer(
        keys.private_key_pem, test_issuer, test_audience);
    auto verifier = jwt_authenticator::create_rs256_verifier(
        keys.public_key_pem, test_issuer, test_audience);

    jwt_claims claims;
    claims.subject = "account-uuid-123";
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = std::chrono::system_clock::now() + std::chrono::hours(1);

    auto token = signer.create_token(claims);
    REQUIRE(token.has_value());

    // Tamper with the signature portion (last segment of the JWT).
    std::string tampered = *token;
    auto dot2 = tampered.rfind('.');
    if (dot2 != std::string::npos && dot2 + 5 < tampered.size()) {
        tampered[dot2 + 3] ^= 1;
    }

    auto validated = verifier.validate(tampered);
    REQUIRE_FALSE(validated.has_value());
}

TEST_CASE("jwt_authenticator_rs256_expired_token", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing RS256 expired token rejected";

    const auto& keys = test_rsa_keys();
    auto signer = jwt_authenticator::create_rs256_signer(
        keys.private_key_pem, test_issuer, test_audience);
    auto verifier = jwt_authenticator::create_rs256_verifier(
        keys.public_key_pem, test_issuer, test_audience);

    jwt_claims claims;
    claims.subject = "account-uuid-123";
    claims.issued_at = std::chrono::system_clock::now() - std::chrono::hours(2);
    claims.expires_at = std::chrono::system_clock::now() - std::chrono::hours(1);

    auto token = signer.create_token(claims);
    REQUIRE(token.has_value());

    auto validated = verifier.validate(*token);
    REQUIRE_FALSE(validated.has_value());
    REQUIRE(validated.error() == jwt_error::expired_token);
}

TEST_CASE("jwt_error_to_string", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing jwt_error to_string";

    REQUIRE(to_string(jwt_error::invalid_token) == "Invalid token");
    REQUIRE(to_string(jwt_error::expired_token) == "Token has expired");
    REQUIRE(to_string(jwt_error::invalid_signature) == "Invalid signature");
    REQUIRE(to_string(jwt_error::missing_claims) == "Missing required claims");
    REQUIRE(to_string(jwt_error::invalid_issuer) == "Invalid issuer");
    REQUIRE(to_string(jwt_error::invalid_audience) == "Invalid audience");
}
