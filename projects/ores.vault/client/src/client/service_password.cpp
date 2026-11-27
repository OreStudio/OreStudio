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
#include "ores.vault.client/client/service_password.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.security/crypto/key_file.hpp"
#include "ores.vault.client/client/credential_bootstrap.hpp"
#include <algorithm>
#include <chrono>
#include <stdexcept>
#include <string>
#include <thread>

namespace ores::vault::client {

namespace {

using namespace ores::logging;

inline static std::string_view password_logger_name = "ores.vault.client.client.service_password";

[[nodiscard]] auto& password_lg() {
    static auto instance = make_logger(password_logger_name);
    return instance;
}

[[nodiscard]] std::optional<std::string> read_variable(const std::string_view name) {
    const auto value = ores::platform::environment::environment::get_value(std::string(name));
    if (!value || value->empty())
        return std::nullopt;
    return *value;
}

}

std::optional<placed_credential> placed_credential_from_environment() {
    const auto consumer = read_variable(consumer_variable);
    const auto secret = read_variable(secret_variable);

    if (!consumer && !secret)
        return std::nullopt;
    if (!consumer || !secret) {
        // The message names the half that is missing, because that is the one
        // the unit's deployment has to add.
        const auto missing = consumer ? secret_variable : consumer_variable;
        throw std::runtime_error("The platform states no " + std::string(missing) +
                                 ", so this unit does not know what to ask the vault for");
    }
    return placed_credential{.consumer = *consumer, .secret = *secret};
}

std::optional<std::string> resolve_placed_password(const ores::nats::config::nats_options& nats,
                                                   const placed_credential& placed) {
    // No key placed means no deployment: a checkout, a test run, or a process
    // that takes its password from the configuration and still works.
    if (!security::crypto::key_file::credential_directory()) {
        ORES_LOG_SEV(password_lg(), debug) << "The platform placed no key, so " << placed.secret
                                           << " is not resolved and the configured password stands";
        return std::nullopt;
    }

    const auto credentials = read_credentials(placed.consumer);

    // The fleet starts as a wave, so the vault may not be subscribed yet when a
    // service asks. The wait is bounded and logged: a vault that never answers
    // delays this service and then leaves it on its configured password rather
    // than waiting for ever.
    ores::nats::service::client client(nats);
    client.connect();

    auto wait = std::chrono::milliseconds(250);
    constexpr auto longest_wait = std::chrono::milliseconds(4000);
    constexpr int attempts = 12;
    for (int attempt = 1;; ++attempt) {
        try {
            const auto password = resolve_credential(client, credentials, placed.secret);
            ORES_LOG_SEV(password_lg(), info)
                << "Resolved " << placed.secret << " for " << placed.consumer;
            return password;
        } catch (const std::runtime_error& e) {
            if (attempt == attempts)
                throw;
            ORES_LOG_SEV(password_lg(), warn)
                << "The vault has not answered for " << placed.consumer << " yet (attempt "
                << attempt << "): " << e.what();
            std::this_thread::sleep_for(wait);
            wait = std::min(wait * 2, longest_wait);
        }
    }
}

}
