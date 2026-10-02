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
#ifndef ORES_REFDATA_CORE_MESSAGING_TENANT_PARTY_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_TENANT_PARTY_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <optional>

namespace ores::refdata::messaging {

/**
 * @brief Serves refdata.v1.parties.list-of-tenant.
 *
 * The handler checks the caller may read parties; the service decides whether
 * the caller and the named tenant are ones the read is served for.
 */
class ORES_REFDATA_CORE_EXPORT tenant_party_handler {
public:
    tenant_party_handler(ores::nats::service::client& nats,
                         ores::database::context ctx,
                         std::optional<ores::security::jwt::jwt_authenticator> verifier);

    void list(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
