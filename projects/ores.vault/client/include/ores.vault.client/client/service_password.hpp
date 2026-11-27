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
#ifndef ORES_VAULT_CLIENT_CLIENT_SERVICE_PASSWORD_HPP
#define ORES_VAULT_CLIENT_CLIENT_SERVICE_PASSWORD_HPP

#include "ores.vault.client/export.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::nats::config {
struct nats_options;
}

namespace ores::vault::client {

/// The variable a unit's deployment sets to the consumer the unit runs as.
inline constexpr std::string_view consumer_variable = "ORES_VAULT_CONSUMER";

/// The variable it sets to the name of the secret holding the unit's password.
inline constexpr std::string_view secret_variable = "ORES_VAULT_PASSWORD_SECRET";

/**
 * @brief The two names a deployment states for a unit that resolves its own
 * password.
 *
 * Both are stated rather than derived from the database user: a unit's database
 * user is a role, and the consumer it runs as is an account, and one is not
 * spelled out of the other.
 */
struct placed_credential final {
    std::string consumer;
    std::string secret;
};

/**
 * @brief The names the platform states for this process.
 *
 * @return The pair, or nothing when the platform states neither, which is the
 * case for a process that takes its password from the configuration.
 *
 * @throws std::runtime_error When the platform states one and not the other: a
 * half-stated unit would resolve the wrong secret or none, and either is worse
 * than refusing to start the exchange.
 */
[[nodiscard]] ORES_VAULT_CLIENT_EXPORT std::optional<placed_credential>
placed_credential_from_environment();

/**
 * @brief Resolves the password the deployment provisioned for this unit.
 *
 * The first secret a workload resolves is its own database password, and it
 * resolves it before any pool exists, so this is the exchange used exactly
 * where secret zero bites: the platform placed a key pair, the vault holds the
 * public half, and a challenge sealed to it proves which consumer is calling.
 *
 * @param nats The transport the request is made over.
 * @param placed The consumer and the secret its deployment named.
 * @return The plaintext password, or nothing when the platform placed no key
 * for this unit, which is what a checkout with no deployment looks like.
 *
 * @throws std::runtime_error When a key is placed and the vault refuses, so a
 * deployment whose vault is unreachable is seen rather than mistaken for one
 * that never provisioned anything.
 */
[[nodiscard]] ORES_VAULT_CLIENT_EXPORT std::optional<std::string>
resolve_placed_password(const ores::nats::config::nats_options& nats,
                        const placed_credential& placed);

}

#endif
