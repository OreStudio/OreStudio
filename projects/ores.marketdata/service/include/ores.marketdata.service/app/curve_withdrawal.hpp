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
#ifndef ORES_MARKETDATA_SERVICE_APP_CURVE_WITHDRAWAL_HPP
#define ORES_MARKETDATA_SERVICE_APP_CURVE_WITHDRAWAL_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.marketdata.service/export.hpp"
#include <string_view>

namespace ores::marketdata::service::app {

/**
 * @brief The wire messages are the component's, not this layer's.
 *
 * The reads answer with the protocol's own types, so the name is stated once
 * here: a service namespace that spells `messaging` resolves to itself, which
 * holds no messages.
 */
namespace messaging = ores::marketdata::messaging;

/**
 * @brief The kind of approval request that permits a curve withdrawal.
 *
 * A withdrawal ends a published set, and the journey states that a tenant may
 * let a trader withdraw alone or require a second pair of eyes, exactly as a
 * publish does. The kind is seeded in
 * projects/ores.sql/populate/inbox/inbox_approval_kinds_populate.sql.
 *
 * The publish gate's single-use rule is untouched by this kind: a publish
 * request authorises one publish, and a withdrawal names a request of another
 * kind, so no request authorises two acts of either sort.
 */
inline constexpr std::string_view curve_withdrawal_kind_code = "marketdata.curve_withdrawal";

/**
 * @brief Takes a published set out of use, as a new version of its record.
 *
 * The act writes four facts and clears nothing: the account, the instant, the
 * reason code and the request that authorised it. The record keeps its publisher
 * and its published instant, so a reader still sees the set a desk priced from
 * and reads beside it that the set left use and why.
 *
 * Refused, with the words a person reads: a set that does not exist, a set that
 * has not been published -- a candidate is not in use -- a set that already
 * ended, an empty reason, and a named request that is not an approved
 * withdrawal request or that already ended a set.
 */
ORES_MARKETDATA_SERVICE_EXPORT messaging::withdraw_curve_response
withdraw_curve(const ores::database::context& ctx,
               const messaging::withdraw_curve_request& request);

}

#endif
