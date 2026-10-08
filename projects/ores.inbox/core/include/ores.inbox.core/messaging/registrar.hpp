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
#ifndef ORES_INBOX_CORE_MESSAGING_REGISTRAR_HPP
#define ORES_INBOX_CORE_MESSAGING_REGISTRAR_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <chrono>
#include <optional>
#include <vector>

namespace ores::inbox::messaging {

/**
 * @brief Registers every inbox NATS handler the service answers.
 */
class ORES_INBOX_CORE_EXPORT registrar {
public:
    /**
     * @brief The window is the queue's answered tail: how long an answered
     * request stays in view. It comes from the installation's setting, which
     * the service reads once, so the handler is handed the value rather than
     * reading a setting on every queue read.
     */
    static std::vector<ores::nats::service::subscription> register_handlers(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier,
        std::chrono::seconds answered_window,
        std::chrono::seconds reminder_window);
};

}

#endif
