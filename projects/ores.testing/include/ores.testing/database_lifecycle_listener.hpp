/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_TESTING_DATABASE_LIFECYCLE_LISTENER_HPP
#define ORES_TESTING_DATABASE_LIFECYCLE_LISTENER_HPP

#include <string>
#include <catch2/reporters/catch_reporter_event_listener.hpp>

namespace ores::testing {

/**
 * @brief Catch2 listener that manages database lifecycle for tests.
 *
 * This listener creates a unique test database when tests start running and
 * cleans it up when tests complete. It only creates databases when tests are
 * actually executed, not during test discovery.
 */
class database_lifecycle_listener : public Catch::EventListenerBase {
public:
    using Catch::EventListenerBase::EventListenerBase;

    /**
     * @brief Called when test run starts - creates the test database.
     */
    void testRunStarting(Catch::TestRunInfo const& testRunInfo) override;

    /**
     * @brief Called when test run ends - drops the test database.
     */
    void testRunEnded(Catch::TestRunStats const& testRunStats) override;

private:
    std::string test_db_name_;
};

}

#endif
