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
#include "ores.logging/make_logger.hpp"
#include "ores.logging/scoped_attribute.hpp"
#include <boost/log/core.hpp>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <string>
#include <string_view>
#include <utility>

namespace {

const std::string_view test_suite("ores.logging.tests");
const std::string tags("[logging]");

const std::string guarded_attribute("OresLoggingScopedAttributeGuarded");
const std::string moved_attribute("OresLoggingScopedAttributeMoved");

bool has_thread_attribute(const std::string& name) {
    const auto attributes(boost::log::core::get()->get_thread_attributes());
    return attributes.find(name) != attributes.end();
}

}

using ores::logging::scoped_attribute;

TEST_CASE("guarded_thread_attribute_exists_only_while_guard_is_alive", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    REQUIRE_FALSE(has_thread_attribute(guarded_attribute));

    {
        const scoped_attribute guard(guarded_attribute);
        BOOST_LOG_SEV(lg, ores::logging::info)
            << "guarded attribute present: " << has_thread_attribute(guarded_attribute);

        CHECK(has_thread_attribute(guarded_attribute));
    }

    CHECK_FALSE(has_thread_attribute(guarded_attribute));
}

TEST_CASE("moved_from_guard_does_not_remove_the_thread_attribute", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    REQUIRE_FALSE(has_thread_attribute(moved_attribute));

    {
        auto source(std::make_unique<scoped_attribute>(moved_attribute));
        const scoped_attribute target(std::move(*source));
        source.reset();

        BOOST_LOG_SEV(lg, ores::logging::info)
            << "attribute survived the moved-from guard: " << has_thread_attribute(moved_attribute);

        CHECK(has_thread_attribute(moved_attribute));
    }

    CHECK_FALSE(has_thread_attribute(moved_attribute));
}
