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
#include "ores.vault.client/client/credential_bootstrap.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/crypto/key_file.hpp"
#include "ores.security/crypto/sealed_box.hpp"
#include "ores.vault.api/messaging/custody_context.hpp"
#include "ores.vault.api/messaging/secret_operations_protocol.hpp"
#include <cstddef>
#include <span>
#include <stdexcept>
#include <utility>
#include <vector>

namespace ores::vault::client {

namespace {

using namespace ores::logging;

inline static std::string_view logger_name = "ores.vault.client.client.credential_bootstrap";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

[[nodiscard]] std::string as_text(const std::span<const std::byte> bytes) {
    if (bytes.empty())
        return {};
    return {reinterpret_cast<const char*>(bytes.data()), bytes.size()};
}

/// What the exchange carries: a subject and the bytes it writes to it.
[[nodiscard]] std::string
over(const request_fn& send, const std::string_view subject, const std::vector<std::byte>& body) {
    return send(subject, as_text(body));
}

}

bootstrap_credentials read_credentials(std::string consumer,
                                       std::string_view private_key_name,
                                       std::string_view transport_key_name) {
    const auto directory = security::crypto::key_file::credential_directory();
    if (!directory) {
        throw std::runtime_error("The platform names no credential directory, so this unit holds "
                                 "no key of its own");
    }

    const auto read_key = [&directory](const std::string_view file_name) {
        return security::crypto::key_file::read(*directory / file_name,
                                                security::crypto::sealed_box::KEY_LEN);
    };

    bootstrap_credentials credentials;
    credentials.consumer = std::move(consumer);
    credentials.private_key = read_key(private_key_name);
    credentials.transport_public_key = read_key(transport_key_name);

    ORES_LOG_SEV(lg(), info) << "Read the bootstrap credentials of " << credentials.consumer;
    return credentials;
}

std::string resolve_credential(const request_fn& send,
                               const bootstrap_credentials& credentials,
                               const std::string& name) {
    const auto& codec = ores::nats::default_wire_codec();

    const auto challenge_answered =
        over(send,
             messaging::challenge_request::nats_subject,
             codec.encode(messaging::challenge_request{.consumer = credentials.consumer}));
    const auto challenge =
        codec.decode<messaging::challenge_response>(std::as_bytes(std::span(challenge_answered)));
    if (!challenge)
        throw std::runtime_error("The answer to the challenge is not one this protocol reads");
    if (!challenge->success)
        throw std::runtime_error("The vault issued no challenge: " + challenge->message);

    // The vault sealed the nonce to this unit's key, so a process that does not
    // hold the private half cannot produce the answer the next step needs.
    const auto nonce = security::crypto::sealed_box::open(
        credentials.private_key,
        challenge->nonce,
        messaging::custody_context::challenge(credentials.consumer, challenge->challenge_id));

    const auto sealed_answer = security::crypto::sealed_box::seal(
        credentials.transport_public_key,
        nonce,
        messaging::custody_context::credential(
            credentials.consumer, challenge->challenge_id, name));

    const auto credential_answered =
        over(send,
             messaging::credential_request::nats_subject,
             codec.encode(messaging::credential_request{.consumer = credentials.consumer,
                                                        .challenge_id = challenge->challenge_id,
                                                        .name = name,
                                                        .sealed_answer = sealed_answer}));
    const auto credential =
        codec.decode<messaging::credential_response>(std::as_bytes(std::span(credential_answered)));
    if (!credential)
        throw std::runtime_error("The answer to the credential is not one this protocol reads");
    if (!credential->success)
        throw std::runtime_error("The vault resolved no secret: " + credential->message);

    const auto value = security::crypto::sealed_box::open(
        credentials.private_key,
        credential->sealed_value,
        messaging::custody_context::delivery(credentials.consumer, name, credential->key_version));

    ORES_LOG_SEV(lg(), info) << "Resolved " << name << " for " << credentials.consumer
                             << " at version " << credential->key_version;
    return value;
}

std::string resolve_credential(ores::nats::service::client& nats,
                               const bootstrap_credentials& credentials,
                               const std::string& name) {
    const auto send_over_bus = [&nats](const std::string_view subject,
                                       const std::string_view body) {
        const std::span<const std::byte> sent(reinterpret_cast<const std::byte*>(body.data()),
                                              body.size());
        const auto reply = nats.request_sync(subject, sent);
        return as_text(reply.data);
    };
    return resolve_credential(request_fn(send_over_bus), credentials, name);
}

}
