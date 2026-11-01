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
#include "ores.refdata.api/domain/tenor_convention.hpp"
#include "ores.refdata.core/repository/tenor_convention_entity.hpp"
#include "ores.refdata.core/repository/tenor_convention_mapper.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::refdata::repository::tenor_convention_mapper;

ores::refdata::domain::tenor_convention convention_stating(const std::string& bdc) {
    ores::refdata::domain::tenor_convention v;
    v.code = "TEST-TENOR";
    v.business_day_convention_type = bdc;
    return v;
}

}

TEST_CASE("a_tenor_convention_stores_an_abbreviated_business_day_convention_by_its_full_name",
          "[tenor_convention][mapper][store_transform]") {
    CHECK(tenor_convention_mapper::map(convention_stating("MF")).business_day_convention_type ==
          "ModifiedFollowing");
    CHECK(tenor_convention_mapper::map(convention_stating("U")).business_day_convention_type ==
          "Unadjusted");
}

TEST_CASE("a_tenor_convention_stores_a_full_business_day_convention_name_unchanged",
          "[tenor_convention][mapper][store_transform]") {
    CHECK(tenor_convention_mapper::map(convention_stating("Following"))
              .business_day_convention_type == "Following");
}

TEST_CASE("a_tenor_convention_stores_an_unknown_business_day_convention_as_stated",
          "[tenor_convention][mapper][store_transform]") {
    CHECK(tenor_convention_mapper::map(convention_stating("NoSuchConvention"))
              .business_day_convention_type == "NoSuchConvention");
}
