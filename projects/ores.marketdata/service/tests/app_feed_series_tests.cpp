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
#include "ores.marketdata.service/app/feed_ingest_loop.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::marketdata::service::app::consumer;
using ores::marketdata::service::app::make_feed_series;

const std::string app_feed_series_eur_usd_spot =
    "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD";
const std::string service_account = "system.test";

consumer make_owner() {
    const boost::uuids::string_generator uuid;
    consumer c;
    c.tenant_id =
        ores::utility::uuid::tenant_id::from_string("11111111-1111-1111-1111-111111111111").value();
    c.party_id = uuid("22222222-2222-2222-2222-222222222222");
    c.source_name = "synthetic.eurusd";
    return c;
}

}

TEST_CASE("the ingest creates the series under the provider of the route it stores the "
          "tick for",
          "[app][feed_series]") {
    auto owner = make_owner();
    owner.provider_code = "bloomberg";

    const auto series = make_feed_series(owner,
                                         boost::uuids::random_generator{}(),
                                         app_feed_series_eur_usd_spot,
                                         "spot",
                                         service_account);

    CHECK(series.provider_code == "bloomberg");
    CHECK(series.oresmd_uri == app_feed_series_eur_usd_spot);
    CHECK(series.series_subclass == "spot");
    CHECK(series.tenant_id == owner.tenant_id);
    CHECK(series.party_id == owner.party_id);
}
