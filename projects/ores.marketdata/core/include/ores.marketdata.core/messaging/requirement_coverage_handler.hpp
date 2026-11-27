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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_REQUIREMENT_COVERAGE_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_REQUIREMENT_COVERAGE_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <optional>

namespace ores::marketdata::messaging {

/**
 * @brief Answers which of a run's oresmd addresses the party's market answers.
 *
 * A run asks before it submits any work, so that a trade whose data is absent is
 * left out rather than failed by the engine. The question is the market data
 * component's because only it knows which series a party holds and which points
 * they carry.
 */
class ORES_MARKETDATA_CORE_EXPORT requirement_coverage_handler {
public:
    requirement_coverage_handler(ores::nats::service::client& nats,
                                 const ores::database::context& ctx,
                                 std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(verifier) {}

    /**
     * @brief Answers the coverage of the addresses the request names.
     *
     * @param msg the encoded request.
     */
    void get_requirement_coverage(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
