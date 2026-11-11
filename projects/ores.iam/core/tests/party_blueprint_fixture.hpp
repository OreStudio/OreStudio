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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_IAM_CORE_TESTS_PARTY_BLUEPRINT_FIXTURE_HPP
#define ORES_IAM_CORE_TESTS_PARTY_BLUEPRINT_FIXTURE_HPP

#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.testing/uuid_of.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>

/**
 * @brief The rows the tests of a party's blueprints share: the system party the parties copy
 * from, and the market series the blueprints name.
 */
namespace ores::iam::tests {

/// The system party, whose blueprints are the templates a party copies.
inline const auto system_party = ores::testing::uuid_of("00000000-0000-4000-8000-000000000001");

/// A market series row of @p party, as the copy plans read it.
inline marketdata::domain::market_series market_series_row(const boost::uuids::uuid& series_id,
                                                           const boost::uuids::uuid& party,
                                                           std::string uri,
                                                           std::string provider,
                                                           std::string subclass = "") {
    marketdata::domain::market_series s;
    s.id = series_id;
    s.party_id = party;
    s.oresmd_uri = std::move(uri);
    s.provider_code = std::move(provider);
    s.series_subclass = std::move(subclass);
    return s;
}

}

#endif
