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
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.trading.core/repository/trade_envelope_additional_field_repository.hpp"
#include "ores.trading.core/repository/trade_envelope_portfolio_id_repository.hpp"
#include "ores.trading.core/repository/trade_envelope_repository.hpp"
#include "ores.trading.core/service/trade_envelope_reader.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[repository]");

using ores::testing::database_helper;
using ores::trading::domain::trade_envelope;
using ores::trading::domain::trade_envelope_additional_field;
using ores::trading::domain::trade_envelope_portfolio_id;
using ores::trading::repository::trade_envelope_additional_field_repository;
using ores::trading::repository::trade_envelope_portfolio_id_repository;
using ores::trading::repository::trade_envelope_repository;
using ores::trading::service::trade_envelope_reader;

struct stamps final {
    boost::uuids::uuid trade_id;
    ores::utility::uuid::tenant_id tenant_id;
    std::string user;
};

template <typename T>
void stamp(T& v, const stamps& s) {
    v.trade_id = s.trade_id;
    v.tenant_id = s.tenant_id;
    v.modified_by = s.user;
    v.performed_by = s.user;
    v.change_reason_code = "system.test";
    v.change_commentary = "Trade envelope reader test";
}

void write_envelope(database_helper& h,
                    const stamps& s,
                    bool has_portfolio_ids,
                    bool has_additional_fields) {
    trade_envelope e;
    stamp(e, s);
    e.counter_party = "ACME";
    e.netting_set_id = "NS-1";
    e.has_portfolio_ids = has_portfolio_ids;
    e.has_additional_fields = has_additional_fields;
    trade_envelope_repository().write(h.context(), e);
}

void write_portfolio_id(database_helper& h,
                        const stamps& s,
                        int sequence_number,
                        const std::string& portfolio_id) {
    trade_envelope_portfolio_id p;
    stamp(p, s);
    p.sequence_number = sequence_number;
    p.portfolio_id = portfolio_id;
    trade_envelope_portfolio_id_repository().write(h.context(), p);
}

void write_additional_field(database_helper& h,
                            const stamps& s,
                            int sequence_number,
                            const std::string& name,
                            const std::string& value) {
    trade_envelope_additional_field f;
    stamp(f, s);
    f.sequence_number = sequence_number;
    f.name = name;
    f.value = value;
    trade_envelope_additional_field_repository().write(h.context(), f);
}

}

TEST_CASE("read_envelopes_rebuilds_lists_in_ordinal_order", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    const stamps s{
        .trade_id = ctx.generate_uuid(),
        .tenant_id = h.tenant_id(),
        .user = ctx.env().get_or(
            std::string(ores::utility::generation::generation_keys::modified_by), "system")};

    write_envelope(h, s, true, true);

    // Written out of ordinal order, so the order read back is the query's
    // doing rather than the insert order's.
    write_portfolio_id(h, s, 3, "P-3");
    write_portfolio_id(h, s, 1, "P-1");
    write_portfolio_id(h, s, 2, "P-2");
    write_additional_field(h, s, 2, "Second", "b");
    write_additional_field(h, s, 1, "First", "a");

    trade_envelope_reader reader(h.context());
    const auto envelopes = reader.read_envelopes({boost::uuids::to_string(s.trade_id)});

    REQUIRE(envelopes.size() == 1);
    const auto& data = envelopes.at(boost::uuids::to_string(s.trade_id));
    REQUIRE(data.counter_party.has_value());
    CHECK(*data.counter_party == "ACME");
    REQUIRE(data.netting_set_id.has_value());
    CHECK(*data.netting_set_id == "NS-1");

    REQUIRE(data.portfolio_ids.has_value());
    CHECK(*data.portfolio_ids == std::vector<std::string>{"P-1", "P-2", "P-3"});

    REQUIRE(data.additional_fields.has_value());
    REQUIRE(data.additional_fields->size() == 2);
    CHECK(data.additional_fields->at(0).name == "First");
    CHECK(data.additional_fields->at(0).value == "a");
    CHECK(data.additional_fields->at(1).name == "Second");
    CHECK(data.additional_fields->at(1).value == "b");
}

TEST_CASE("read_envelopes_keeps_stated_empty_apart_from_omitted", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    const stamps s{
        .trade_id = ctx.generate_uuid(),
        .tenant_id = h.tenant_id(),
        .user = ctx.env().get_or(
            std::string(ores::utility::generation::generation_keys::modified_by), "system")};

    // Both lists are stated, and both are empty. No child row exists for
    // either, so only the parent's flags can say they were stated.
    write_envelope(h, s, true, true);

    trade_envelope_reader reader(h.context());
    const auto envelopes = reader.read_envelopes({boost::uuids::to_string(s.trade_id)});

    REQUIRE(envelopes.size() == 1);
    const auto& data = envelopes.at(boost::uuids::to_string(s.trade_id));
    REQUIRE(data.portfolio_ids.has_value());
    CHECK(data.portfolio_ids->empty());
    REQUIRE(data.additional_fields.has_value());
    CHECK(data.additional_fields->empty());
}

TEST_CASE("read_envelopes_omits_a_trade_that_stated_no_envelope", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    const stamps written{
        .trade_id = ctx.generate_uuid(),
        .tenant_id = h.tenant_id(),
        .user = ctx.env().get_or(
            std::string(ores::utility::generation::generation_keys::modified_by), "system")};
    const auto unwritten = ctx.generate_uuid();

    write_envelope(h, written, false, false);

    trade_envelope_reader reader(h.context());
    const auto envelopes = reader.read_envelopes(
        {boost::uuids::to_string(written.trade_id), boost::uuids::to_string(unwritten)});

    REQUIRE(envelopes.size() == 1);
    CHECK(envelopes.contains(boost::uuids::to_string(written.trade_id)));
    CHECK(!envelopes.contains(boost::uuids::to_string(unwritten)));

    const auto& data = envelopes.at(boost::uuids::to_string(written.trade_id));
    CHECK(!data.portfolio_ids.has_value());
    CHECK(!data.additional_fields.has_value());
}

TEST_CASE("read_envelopes_batches_more_than_one_trade", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    const auto user = ctx.env().get_or(
        std::string(ores::utility::generation::generation_keys::modified_by), "system");

    const stamps first{.trade_id = ctx.generate_uuid(), .tenant_id = h.tenant_id(), .user = user};
    const stamps second{.trade_id = ctx.generate_uuid(), .tenant_id = h.tenant_id(), .user = user};

    write_envelope(h, first, true, false);
    write_portfolio_id(h, first, 1, "F-1");
    write_envelope(h, second, true, false);
    write_portfolio_id(h, second, 1, "S-1");
    write_portfolio_id(h, second, 2, "S-2");

    trade_envelope_reader reader(h.context());
    const auto envelopes = reader.read_envelopes(
        {boost::uuids::to_string(first.trade_id), boost::uuids::to_string(second.trade_id)});

    REQUIRE(envelopes.size() == 2);

    const auto& a = envelopes.at(boost::uuids::to_string(first.trade_id));
    REQUIRE(a.portfolio_ids.has_value());
    CHECK(*a.portfolio_ids == std::vector<std::string>{"F-1"});

    const auto& b = envelopes.at(boost::uuids::to_string(second.trade_id));
    REQUIRE(b.portfolio_ids.has_value());
    CHECK(*b.portfolio_ids == std::vector<std::string>{"S-1", "S-2"});
}
