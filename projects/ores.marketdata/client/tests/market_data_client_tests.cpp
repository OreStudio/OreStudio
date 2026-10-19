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
#include "ores.marketdata.client/market_data_client.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[market_data_client]");

using ores::marketdata::client::feed_binding_write_of;
using ores::marketdata::domain::feed_binding;

feed_binding make_binding() {
    feed_binding b;
    b.id = boost::uuids::random_generator()();
    b.party_id = boost::uuids::random_generator()();
    b.source_name = "synthetic.bloomberg.eurusd";
    b.producer_kind = "SYNTHETIC";
    b.enabled = true;
    return b;
}

}

TEST_CASE("a_saved_binding_names_its_producer_kind_on_the_wire", tags) {
    const auto binding = make_binding();

    const auto write = feed_binding_write_of(binding);

    CHECK(write.producer_kind == "SYNTHETIC");
}

TEST_CASE("a_saved_binding_carries_every_field_the_binding_owns", tags) {
    const auto binding = make_binding();

    const auto write = feed_binding_write_of(binding);

    CHECK(write.id == binding.id);
    CHECK(write.party_id == binding.party_id);
    CHECK(write.source_name == binding.source_name);
    CHECK(write.enabled == binding.enabled);
}

TEST_CASE("a_binding_that_keeps_the_default_producer_kind_sends_the_default", tags) {
    feed_binding binding;
    binding.id = boost::uuids::random_generator()();

    const auto write = feed_binding_write_of(binding);

    CHECK(write.producer_kind == "VENDOR");
}
