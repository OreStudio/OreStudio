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
#ifndef ORES_MARKETDATA_SERVICE_APP_CURVE_NOTICES_HPP
#define ORES_MARKETDATA_SERVICE_APP_CURVE_NOTICES_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/curve_publish.hpp"
#include "ores.marketdata.service/export.hpp"
#include <chrono>
#include <optional>
#include <string>

namespace ores::marketdata::service::app {

/**
 * @brief The notification kinds a change to a published set is told with.
 *
 * A component seeds the kinds it raises in its own populate script, so these two
 * live beside the market data catalogue rather than with the inbox's own kinds.
 */
inline constexpr std::string_view curve_published_kind_code = "marketdata.curve_published";
inline constexpr std::string_view curve_withdrawn_kind_code = "marketdata.curve_withdrawn";

/**
 * @brief Tells the readers of a curve that a new version is live.
 *
 * The trigger is the apply, once, at the instant the published set moves: until
 * somebody applies an approval the curve the system uses has not changed, and a
 * notice that said it had would be untrue. A replacement sends the same notice,
 * because a replacement is a publish.
 *
 * The notice names the subject and the act and never the values: the curve, the
 * party, the as-of it was built at, the instant it moved, the publisher, the
 * request it stands on and the as-of it replaced. The link opens the published
 * curve's record page, which is where the values are read.
 *
 * The recipients are the people who read the curve -- the accounts of the party
 * that holds it and the accounts of the tenant that may read market series --
 * because the bell hangs on an account and the subscription names a party. The
 * consuming services are not reachable this way, and the header states why.
 *
 * Telling is never the publish: a failure here is logged and swallowed, so a set
 * that moved is not unmoved by a notice that could not be written.
 *
 * @param previous_as_of The as-of of the set this one replaced, or nothing when
 * it is the first the series ever had.
 */
ORES_MARKETDATA_SERVICE_EXPORT void
announce_publish(const ores::database::context& ctx,
                 const domain::curve_publish& published,
                 std::optional<std::chrono::system_clock::time_point> previous_as_of);

/**
 * @brief Tells the readers of a curve that a published set left use.
 *
 * A withdrawal is the change a reader must act on, so it is its own kind and it
 * carries the reason in words. The subject is the same one the publish notice
 * names, and the link opens the same record.
 *
 * @param previous_as_of The set's own as-of, which is the version that left use.
 */
ORES_MARKETDATA_SERVICE_EXPORT void
announce_withdrawal(const ores::database::context& ctx,
                    const domain::curve_publish& withdrawn,
                    std::optional<std::chrono::system_clock::time_point> previous_as_of);

}
#endif
