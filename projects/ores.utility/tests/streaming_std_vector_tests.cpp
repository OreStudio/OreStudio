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
#include "ores.utility/streaming/std_vector.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

namespace {

/**
 * @brief A value with no wire shape, which prints itself.
 *
 * A test framework prints an expected value it does not match, and most of the
 * domain types it prints have a shape. This one does not, which is what the
 * printer's second branch is for.
 */
struct prints_itself {
    std::string text;

    friend std::ostream& operator<<(std::ostream& s, const prints_itself& v) {
        return s << v.text;
    }
};

}

TEST_CASE("a vector of a type the codec writes whole is one JSON array", "[streaming]") {
    const std::vector<boost::uuids::uuid> ids{
        boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
        boost::uuids::string_generator{}("6ba7b810-9dad-11d1-80b4-00c04fd430c8")};

    std::ostringstream s;
    s << ids;

    CHECK(s.str() ==
          R"(["3f2504e0-4f89-11d3-9a0c-0305e82c3301","6ba7b810-9dad-11d1-80b4-00c04fd430c8"])");
}

TEST_CASE("a vector of a type that prints itself writes each element's own text", "[streaming]") {
    const std::vector<prints_itself> values{{"one"}, {"two"}};

    std::ostringstream s;
    s << values;

    CHECK(s.str() == "[one,two]");
}

TEST_CASE("a vector of no element at all is an empty array", "[streaming]") {
    std::ostringstream s;
    s << std::vector<prints_itself>{};

    CHECK(s.str() == "[]");
}
