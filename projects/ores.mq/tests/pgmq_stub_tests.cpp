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
#include <catch2/catch_test_macros.hpp>
#include "ores.mq/pgmq/stub.hpp"
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.mq.tests");
const std::string tags("[pgmq]");

}

using namespace ores::mq::pgmq;
using namespace ores::logging;

TEST_CASE("pgmq_stub_compiles", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, warn) << stub_function();
    stub s;
    CHECK(s.version == 0);
}
