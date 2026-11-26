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
#ifndef ORES_REFDATA_CORE_MESSAGING_BUNDLE_IMPORT_OPERATIONS_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_BUNDLE_IMPORT_OPERATIONS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <optional>

namespace ores::refdata::messaging {

/**
 * @brief NATS message handler for the bundle import's three operations.
 *
 * The handler owns the session, the permission and the wire; what each
 * operation decides is service::bundle_import_operations_service, which a test
 * drives without NATS.
 */
class ORES_REFDATA_CORE_EXPORT bundle_import_operations_handler {
public:
    bundle_import_operations_handler(
        ores::nats::service::client& nats,
        const ores::database::context& ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier);

    /**
     * @brief Opens a staging session, or answers the one the id already names.
     */
    void begin(ores::nats::message msg);

    /**
     * @brief Stages one family payload against an open import.
     */
    void stage(ores::nats::message msg);

    /**
     * @brief Applies every staged family of one import in one transaction.
     */
    void commit(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
