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
#include "ores.reporting.api/domain/report_definition_template.hpp"
#include "ores.reporting.api/domain/report_definition_template_json_io.hpp"
#include <catch2/catch_test_macros.hpp>
#include <sstream>
#include <string>

// This is the component's one hand-written wire type, and the JSON is the shape
// the DQ service is read through, so the test asserts the values appear rather
// than that anything non-empty was produced.

using ores::reporting::domain::report_definition_template;

namespace {
const std::string tags("[domain][template]");
}

TEST_CASE("a template streams the values it was given", tags) {
    report_definition_template sut;
    sut.name = "Capital Report";
    sut.description = "Regulatory capital";
    sut.report_type = "risk";
    sut.schedule_expression = "0 6 * * 1-5";
    sut.concurrency_policy = "skip";
    sut.display_order = 7;

    std::ostringstream os;
    os << sut;
    const auto json = os.str();

    CHECK(json.find("Capital Report") != std::string::npos);
    CHECK(json.find("risk") != std::string::npos);
    CHECK(json.find("0 6 * * 1-5") != std::string::npos);
    CHECK(json.find("skip") != std::string::npos);
}

TEST_CASE("two templates with different names stream differently", tags) {
    report_definition_template a;
    a.name = "A";
    report_definition_template b;
    b.name = "B";

    std::ostringstream osa;
    osa << a;
    std::ostringstream osb;
    osb << b;

    CHECK(osa.str() != osb.str());
}
