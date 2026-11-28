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
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.security/crypto/key_file.hpp"
#include "ores.security/crypto/sealed_box.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_environment_override.hpp"
#include "ores.vault.api/messaging/custody_context.hpp"
#include "ores.vault.api/messaging/secret_operations_protocol.hpp"
#include "ores.vault.client/client/credential_bootstrap.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <filesystem>
#include <fstream>
#include <span>
#include <stdexcept>
#include <string>
#include <vector>

using namespace ores::vault::client;
using Catch::Matchers::ContainsSubstring;

namespace {

namespace messaging = ores::vault::messaging;
using ores::security::crypto::sealed_box;

constexpr std::string_view consumer_name = "iam_service@system.ores";
constexpr std::string_view secret_name = "iam_service.db_password";
constexpr std::string_view secret_value = "the password the deployment resolved";
constexpr std::string_view challenge_id = "6F1E1B0E-4C0B-4E4A-9E18-0C7A9A1B2C3D";
constexpr std::string_view nonce = "the nonce the vault draws and keeps";
constexpr int key_version = 7;

[[nodiscard]] std::string encode(const auto& message) {
    const auto bytes = ores::nats::default_wire_codec().encode(message);
    return std::string(reinterpret_cast<const char*>(bytes.data()), bytes.size());
}

[[nodiscard]] std::span<const std::byte> as_bytes(const std::string_view text) {
    return {reinterpret_cast<const std::byte*>(text.data()), text.size()};
}

/**
 * @brief A party that answers the protocol the way the vault does.
 *
 * It holds a transport key pair, the worked unit's public key and one secret,
 * and it checks the answer the way the vault checks it: by opening the answer
 * with its own transport key. The exchange is therefore proved against a party
 * that carries out the protocol, not against a mock that agrees with whatever
 * the client sends.
 */
struct vault_stub {
    sealed_box::key_pair transport = sealed_box::generate();
    sealed_box::key_pair workload = sealed_box::generate();

    sealed_box::key_pair another_workload = sealed_box::generate();
    std::string refusal;
    bool value_sealed_to_another_workload = false;

    bool answer_opened = false;
    std::string answer_text;
    int challenges_issued = 0;
    int credentials_asked = 0;

    [[nodiscard]] bootstrap_credentials workload_credentials() const {
        return bootstrap_credentials{.consumer = std::string(consumer_name),
                                     .private_key = workload.private_key,
                                     .transport_public_key = transport.public_key};
    }

    [[nodiscard]] std::string answer(const std::string_view subject, const std::string_view body) {
        const auto& codec = ores::nats::default_wire_codec();

        if (subject == messaging::challenge_request::nats_subject) {
            const auto request = codec.decode<messaging::challenge_request>(as_bytes(body));
            if (!request || request->consumer != consumer_name || !refusal.empty()) {
                return encode(
                    messaging::challenge_response{.success = false, .message = "no such consumer"});
            }
            ++challenges_issued;
            return encode(messaging::challenge_response{
                .success = true,
                .challenge_id = std::string(challenge_id),
                .nonce = sealed_box::seal(workload.public_key,
                                          std::string(nonce),
                                          messaging::custody_context::challenge(
                                              request->consumer, std::string(challenge_id)))});
        }

        if (subject == messaging::credential_request::nats_subject) {
            const auto request = codec.decode<messaging::credential_request>(as_bytes(body));
            if (!request) {
                return encode(messaging::credential_response{.success = false,
                                                             .message = "unreadable request"});
            }
            ++credentials_asked;
            answer_text =
                sealed_box::open(transport.private_key,
                                 request->sealed_answer,
                                 messaging::custody_context::credential(
                                     request->consumer, request->challenge_id, request->name));
            answer_opened = answer_text == nonce;
            if (request->name != secret_name) {
                return encode(messaging::credential_response{
                    .success = false, .message = "This secret belongs to another consumer"});
            }
            const auto& recipient = value_sealed_to_another_workload ? another_workload : workload;
            return encode(messaging::credential_response{
                .success = true,
                .name = std::string(secret_name),
                .key_version = key_version,
                .sealed_value = sealed_box::seal(
                    recipient.public_key,
                    std::string(secret_value),
                    messaging::custody_context::delivery(
                        std::string(consumer_name), std::string(secret_name), key_version))});
        }

        return encode(messaging::credential_response{.success = false, .message = "no route"});
    }
};

}

TEST_CASE("a_workload_resolves_its_own_secret", "[client]") {
    vault_stub vault;
    const auto send = [&vault](const std::string_view subject, const std::string_view body) {
        return vault.answer(subject, body);
    };

    const auto resolved =
        resolve_credential(send, vault.workload_credentials(), std::string(secret_name));

    REQUIRE(resolved == secret_value);
    REQUIRE(vault.challenges_issued == 1);
    REQUIRE(vault.credentials_asked == 1);
    REQUIRE(vault.answer_opened);
}

TEST_CASE("a_workload_without_the_key_answers_nothing", "[client]") {
    vault_stub vault;
    const auto send = [&vault](const std::string_view subject, const std::string_view body) {
        return vault.answer(subject, body);
    };

    auto credentials = vault.workload_credentials();
    credentials.private_key = vault.another_workload.private_key;

    REQUIRE_THROWS_AS(resolve_credential(send, credentials, std::string(secret_name)),
                      std::runtime_error);
    REQUIRE_FALSE(vault.answer_opened);
}

TEST_CASE("a_refused_challenge_is_reported_with_its_reason", "[client]") {
    vault_stub vault;
    vault.refusal = "The vault holds no enabled consumer by that name";
    const auto send = [&vault](const std::string_view subject, const std::string_view body) {
        return vault.answer(subject, body);
    };

    REQUIRE_THROWS_WITH(
        resolve_credential(send, vault.workload_credentials(), std::string(secret_name)),
        ContainsSubstring("The vault issued no challenge"));
}

TEST_CASE("a_value_sealed_to_another_workload_does_not_open", "[client]") {
    vault_stub vault;
    vault.value_sealed_to_another_workload = true;
    const auto send = [&vault](const std::string_view subject, const std::string_view body) {
        return vault.answer(subject, body);
    };

    REQUIRE_THROWS_AS(
        resolve_credential(send, vault.workload_credentials(), std::string(secret_name)),
        std::runtime_error);
}

TEST_CASE("the_credentials_come_from_the_named_directory", "[client]") {
    const auto directory =
        ores::testing::project_root::resolve("tmp") /
        ("vault_client_credentials_" + boost::uuids::to_string(boost::uuids::random_generator()()));
    std::filesystem::create_directories(directory);
    const auto keys = sealed_box::generate();
    const auto transport = sealed_box::generate();
    const auto write = [&directory](const std::string_view name,
                                    const std::vector<unsigned char>& bytes) {
        std::ofstream out(directory / name, std::ios::binary);
        out.write(reinterpret_cast<const char*>(bytes.data()),
                  static_cast<std::streamsize>(bytes.size()));
    };
    write(private_key_credential, keys.private_key);
    write(transport_key_credential, transport.public_key);

    {
        const ores::testing::scoped_environment_override environment(
            {{std::string(ores::security::crypto::key_file::directory_variable),
              directory.string()}});

        const auto credentials = read_credentials(std::string(consumer_name));

        REQUIRE(credentials.consumer == consumer_name);
        REQUIRE(credentials.private_key == keys.private_key);
        REQUIRE(credentials.transport_public_key == transport.public_key);
    }

    std::filesystem::remove_all(directory);
}

TEST_CASE("a_unit_with_no_placed_key_is_refused", "[client]") {
    const ores::testing::scoped_environment_override environment(
        {}, {std::string(ores::security::crypto::key_file::directory_variable)});

    REQUIRE_THROWS_WITH(read_credentials(std::string(consumer_name)),
                        ContainsSubstring("names no credential directory"));
}
