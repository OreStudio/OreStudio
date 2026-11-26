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
#include "ores.logging/logger.hpp"
#include "ores.security/crypto/sealed_box.hpp"
#include "ores.security/crypto/secret_sealer.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "ores.vault.api/domain/consumer.hpp"
#include "ores.vault.core/repository/consumer_repository.hpp"
#include "ores.vault.core/repository/secret_use_repository.hpp"
#include "ores.vault.core/security/credential_source.hpp"
#include "ores.vault.api/messaging/custody_context.hpp"
#include "ores.vault.core/service/custody_service.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <vector>

namespace {

using ores::security::crypto::sealed_box;
using ores::security::crypto::secret_sealer;
using ores::testing::scoped_database_helper;
using ores::utility::convert::base64_converter;
using ores::vault::repository::consumer_repository;
using ores::vault::repository::secret_use_repository;
using ores::vault::security::key_material;
using ores::vault::service::custody_service;

const std::string secret_name = "ores.iam.provider.client_secret";
const std::string plaintext = "the-provider-client-secret";

/// A consumer name no other run has used: the database outlives one test case,
/// and a consumer's name is unique within its tenant.
std::string unique_subject() {
    boost::uuids::random_generator generate;
    return "ores.http." + boost::uuids::to_string(generate()).substr(0, 8);
}

/// A key of the length the sealer takes, with a counter in its bytes.
std::vector<unsigned char> make_key(unsigned char seed) {
    std::vector<unsigned char> key(secret_sealer::KEY_LEN);
    for (std::size_t i = 0; i < key.size(); ++i)
        key[i] = static_cast<unsigned char>(seed + i);
    return key;
}

/// One key set, as the deployment would place it: a current version and, when
/// a rotation is under test, the version it replaced.
key_material keys_with(const std::vector<int>& versions, int current) {
    key_material keys;
    for (const auto version : versions)
        keys.key_encryption_keys.emplace(version, make_key(static_cast<unsigned char>(version)));
    keys.current_version = current;
    const auto transport = sealed_box::generate();
    keys.transport_private_key = transport.private_key;
    keys.transport_public_key = transport.public_key;
    return keys;
}

/// Registers the consumer that may resolve the secret, with a key of its own.
sealed_box::key_pair register_consumer(const ores::database::context& ctx,
                                       const std::string& subject) {
    const auto pair = sealed_box::generate();

    ores::vault::domain::consumer consumer;
    boost::uuids::random_generator generate;
    consumer.id = generate();
    consumer.tenant_id = ctx.tenant_id();
    consumer.name = subject;
    consumer.public_key = base64_converter::convert(pair.public_key);
    consumer.is_enabled = true;
    consumer.description = "the consumer under test";

    consumer_repository repo;
    repo.write(ctx, consumer);
    return pair;
}

/// Seals a value the way a caller does: to the vault's transport key, under the
/// context the vault expects for a write.
std::string
seal_for_the_vault(const key_material& keys, const std::string& name, const std::string& owner) {
    return sealed_box::seal(
        keys.transport_public_key, plaintext, ores::vault::messaging::custody_context::write(name, owner));
}

}

TEST_CASE("a_secret_is_written_described_and_resolved", "[vault][custody][integration]") {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    const auto& ctx = h.context();
    const auto keys = keys_with({1}, 1);
    const auto subject = unique_subject();
    const auto consumer = register_consumer(ctx, subject);

    custody_service custody(ctx, keys);

    const auto written = custody.write(
        subject, secret_name, subject, seal_for_the_vault(keys, secret_name, subject));
    ORES_LOG_SEV(lg, info) << "Write answered: " << written.message;
    REQUIRE(written.success);
    CHECK(written.key_version == 1);

    const auto described = custody.describe(secret_name);
    REQUIRE(described.success);
    CHECK(described.owner == subject);
    CHECK(described.key_version == 1);
    CHECK(described.sealed_value.empty());

    const auto resolved = custody.resolve(subject, secret_name);
    REQUIRE(resolved.success);
    CHECK(resolved.key_version == 1);

    // The answer is sealed to the consumer's registered key, under the delivery
    // the vault names; the plaintext is what the caller sealed.
    const auto opened = sealed_box::open(
        consumer.private_key, resolved.sealed_value, subject + "|" + secret_name + "|1");
    CHECK(opened == plaintext);
}

TEST_CASE("a_consumer_that_does_not_own_a_secret_is_refused", "[vault][custody][integration]") {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    const auto& ctx = h.context();
    const auto keys = keys_with({1}, 1);
    const auto subject = unique_subject();
    const auto other = unique_subject();
    register_consumer(ctx, other);

    custody_service custody(ctx, keys);
    REQUIRE(
        custody.write(subject, secret_name, subject, seal_for_the_vault(keys, secret_name, subject))
            .success);

    const auto refused = custody.resolve(other, secret_name);
    CHECK_FALSE(refused.success);
    CHECK(refused.sealed_value.empty());

    const auto unknown = custody.resolve(subject, "a.secret.that.is.not.there");
    CHECK_FALSE(unknown.success);

    // The record is what an operator reads, so both refusals are rows in it.
    secret_use_repository uses;
    const auto rows = uses.read_latest(ctx);
    const auto recorded = [&rows](const std::string& outcome) {
        for (const auto& row : rows)
            if (row.outcome == outcome)
                return true;
        return false;
    };
    CHECK(recorded("refused_not_owner"));
    CHECK(recorded("refused_unknown_secret"));
}

TEST_CASE("a_rotation_re_wraps_a_value_and_leaves_it_readable", "[vault][custody][integration]") {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    const auto& ctx = h.context();
    const auto before = keys_with({1}, 1);
    const auto subject = unique_subject();
    const auto consumer = register_consumer(ctx, subject);

    custody_service first(ctx, before);
    REQUIRE(
        first.write(subject, secret_name, subject, seal_for_the_vault(before, secret_name, subject))
            .success);

    // The deployment has added version 2 and named it current; the rotation is
    // what moves the stored rows onto it.
    auto after = keys_with({1, 2}, 2);
    after.transport_private_key = before.transport_private_key;
    after.transport_public_key = before.transport_public_key;

    custody_service rotated(ctx, after);
    const auto rotation = rotated.rotate(subject);
    ORES_LOG_SEV(lg, info) << "Rotation answered: " << rotation.message;
    REQUIRE(rotation.success);
    CHECK(rotation.rotated == 1);
    CHECK(rotation.key_version == 2);

    const auto resolved = rotated.resolve(subject, secret_name);
    REQUIRE(resolved.success);
    CHECK(resolved.key_version == 2);
    CHECK(sealed_box::open(consumer.private_key,
                           resolved.sealed_value,
                           subject + "|" + secret_name + "|2") == plaintext);
}
