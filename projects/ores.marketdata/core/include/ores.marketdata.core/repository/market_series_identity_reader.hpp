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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_SERIES_IDENTITY_READER_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_SERIES_IDENTITY_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Resolves a series from its typed identity, by narrowing in the store
 * and then reading the candidates through the codec.
 *
 * The read stays out of the generated market_series repository because of how
 * it answers the one question that repository cannot: what a field inside a
 * series URI means. Those fields sit in the row's schema order, which differs
 * between instrument types, so SQL cannot compare them and a pattern match
 * would be wrong. The read therefore does two things instead:
 *
 * - It narrows on =oresmd_uri= with the prefix
 *   =oresmd_uri_codec::series_prefix= writes for the asset class, the scope,
 *   the instrument type and the quote type, which the URI grammar puts in a
 *   fixed position. That prefix is the codec's own spelling of the identity, so
 *   the narrowing states no grammar of its own, and the identity index can
 *   serve it.
 * - It then reads each surviving row's URI back through
 *   =oresmd_uri_codec::read= and drops the ones whose remaining fields do not
 *   hold what the request stated. Those fields, such as =ccy=, are matched by
 *   the codec, never by a pattern.
 *
 * A request the codec cannot spell is refused with =std::invalid_argument=: an
 * unknown instrument or quote type, an unknown field name, and a field the
 * type's row does not declare or may not leave empty. Nothing is guessed, so a
 * malformed request cannot silently resolve to the wrong series.
 */
class ORES_MARKETDATA_CORE_EXPORT market_series_identity_reader final {
public:
    /**
     * @brief The series that carry @p identity, at most one per owning party.
     *
     * @throws std::invalid_argument when the codec refuses the identity.
     */
    [[nodiscard]] static std::vector<domain::market_series>
    read(ores::database::context ctx,
         const messaging::resolve_series_identity_request& identity);
};

}

#endif
