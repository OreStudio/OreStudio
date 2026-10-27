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
#ifndef ORES_MARKETDATA_SERVICE_APP_PUBLISHED_CURVE_READER_HPP
#define ORES_MARKETDATA_SERVICE_APP_PUBLISHED_CURVE_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"

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
 * @brief The published sets a reader finds, and the points each of them holds.
 *
 * A published set is the publish record of the request that authorised it, so
 * both reads start there. The list answers the tenant's sets together, which no
 * read did before: a reader found the set in use for one series and never the
 * sets of a curve family, a party or a tenant. The set read answers one series'
 * last sets with the pillars each holds, which is what the overlay and the
 * comparison draw.
 *
 * The party is named by the read rather than by the caller because a party is a
 * reference-data record: the publish record carries the party's id, and the
 * short code a person recognises is read from the party repository, so this
 * code states no join into another component's tables.
 *
 * The points are the market observations the publish wrote at its own as-of.
 * They travel with the set because the curve snapshot read serves the market as
 * it stands at the caller's clock, and a set built at an earlier as-of is not
 * that.
 */

/**
 * @brief The published sets the request's filters select, newest first.
 *
 * A candidate and an ended set are drawn only when the request asks for them,
 * because a candidate read as the set a desk prices from would be a lie. The
 * total is the size of the whole filtered set, so a screen pages without a
 * second read.
 */
messaging::list_published_curves_response
list_published_curves(const ores::database::context& ctx,
                      const messaging::list_published_curves_request& request);

/**
 * @brief One series' last published sets, oldest first, with the points.
 *
 * The published sets are counted from the newest, so the caller asks for the
 * number it draws. The candidates follow them, newest as-of first, and are not
 * counted against the limit. An ended set is returned like any other published
 * set, because it was published; its state says that it is not the one in use.
 */
messaging::get_published_curve_sets_response
get_published_curve_sets(const ores::database::context& ctx,
                         const messaging::get_published_curve_sets_request& request);

}

#endif
