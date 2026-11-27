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
#ifndef ORES_VAULT_CLIENT_CLIENT_CREDENTIAL_BOOTSTRAP_HPP
#define ORES_VAULT_CLIENT_CLIENT_CREDENTIAL_BOOTSTRAP_HPP

#include "ores.vault.client/export.hpp"
#include <cstdint>
#include <functional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::nats::service {
class client;
}

namespace ores::vault::client {

/**
 * @brief What a workload holds before it holds a token.
 *
 * The deployment places both halves of this: the consumer name it registered
 * for the unit, the unit's own private key, and the vault's transport public
 * key. Nothing here is a shared secret, and none of it can be forged into
 * another unit's identity, because the vault seals to the public key the
 * deployment registered for that name.
 */
struct bootstrap_credentials {
    /// The consumer the deployment registered for this unit, as the vault
    /// names it.
    std::string consumer;

    /// The unit's own private key, as the raw 32 bytes the box takes.
    std::vector<unsigned char> private_key;

    /// The vault's transport public key, as the raw 32 bytes the box takes.
    /// A public key is not a secret.
    std::vector<unsigned char> transport_public_key;
};

/// The file name the deployment uses for the unit's private key.
inline constexpr std::string_view private_key_credential = "vault-consumer.key";

/// The file name the deployment uses for the vault's transport public key.
inline constexpr std::string_view transport_key_credential = "vault-transport.pub";

/**
 * @brief Reads the two credentials the deployment placed for this unit.
 *
 * The files are read from the directory the platform names in the environment,
 * which the unit cannot write, so the key is one no build generated and no
 * environment variable carried.
 *
 * @param consumer The consumer the deployment registered for this unit.
 * @param private_key_name The credential file holding the private key.
 * @param transport_key_name The credential file holding the vault's transport
 * public key.
 * @return The material, ready for @ref resolve_credential.
 *
 * @throws std::runtime_error If the platform names no credential directory, or
 * a file is absent, or either key is not the length the cipher accepts.
 */
[[nodiscard]] ORES_VAULT_CLIENT_EXPORT bootstrap_credentials
read_credentials(std::string consumer,
                 std::string_view private_key_name = private_key_credential,
                 std::string_view transport_key_name = transport_key_credential);

/**
 * @brief One request and its answer, as the exchange sees them.
 *
 * The exchange knows a subject and a body, so stating that much keeps the proof
 * of identity independent of the bus that carries it, and lets a party that
 * answers the protocol in a test stand in for the vault.
 */
using request_fn = std::function<std::string(std::string_view subject, std::string_view body)>;

/**
 * @brief Resolves one of this workload's own secrets before any token exists.
 *
 * The exchange has four steps. It asks the vault for a challenge, opens the
 * nonce the vault sealed to this unit's key, answers that nonce sealed to the
 * vault's transport key, and opens the secret the vault delivers sealed to the
 * same key. Every message is bound to the consumer, the challenge and the
 * secret it belongs to, so a captured message replayed anywhere else does not
 * open.
 *
 * The name is not a request for any secret: the vault answers what the consumer
 * it proved itself to be owns, and refuses the rest as a secret it does not
 * hold. Asking for another's entry buys nothing.
 *
 * @param send Carries one request and returns its reply.
 * @param credentials This unit's material.
 * @param name The secret to resolve, such as the unit's database password.
 * @return The plaintext.
 *
 * @throws std::runtime_error If the vault refuses, or answers something this
 * unit's key does not open.
 */
[[nodiscard]] ORES_VAULT_CLIENT_EXPORT std::string resolve_credential(
    const request_fn& send, const bootstrap_credentials& credentials, const std::string& name);

/**
 * @brief Resolves one of this workload's own secrets over the bus.
 *
 * The same exchange as @ref resolve_credential, carried by the client.
 *
 * @param nats A connected client, which must outlive the call.
 * @param credentials This unit's material.
 * @param name The secret to resolve.
 * @return The plaintext.
 *
 * @throws std::runtime_error If the vault refuses, or the bus fails.
 */
[[nodiscard]] ORES_VAULT_CLIENT_EXPORT std::string
resolve_credential(ores::nats::service::client& nats,
                   const bootstrap_credentials& credentials,
                   const std::string& name);

}

#endif
