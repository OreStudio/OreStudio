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
#include "ores.telemetry/log/lifecycle_manager.hpp"
#include "ores.telemetry/log/database_sink_utils.hpp"
#include "ores.logging/make_logger.hpp"

#include <iostream>
#include <memory>
#include <functional>

// This is a conceptual example showing how the database logging infrastructure
// would be used in practice, particularly for unit testing scenarios.

int main() {
    // Example: Setting up logging with database sink for unit testing
    
    // 1. Configure basic logging options
    ores::logging::logging_options opts;
    opts.severity = "debug";
    opts.output_to_console = true;
    opts.filename = "test_output.log";
    
    // 2. Create the lifecycle manager
    ores::telemetry::log::lifecycle_manager lm(std::make_optional(opts));
    
    // 3. Create a resource descriptor for this test application
    auto resource = std::make_shared<ores::telemetry::domain::resource>(
        ores::telemetry::domain::resource::from_environment("test-app", "1.0.0"));
    
    // 4. Create a mock handler that simulates database storage
    // In a real scenario, this would connect to the actual telemetry repository
    auto mock_db_handler = [](const ores::telemetry::domain::telemetry_log_entry& entry) {
        std::cout << "Mock DB Storage - Log Entry:" << std::endl;
        std::cout << "  Timestamp: " << entry.timestamp.time_since_epoch().count() << std::endl;
        std::cout << "  Level: " << entry.level << std::endl;
        std::cout << "  Component: " << entry.component << std::endl;
        std::cout << "  Message: " << entry.message << std::endl;
        std::cout << "  Source: " << entry.source << std::endl;
        std::cout << "  Source Name: " << entry.source_name << std::endl;
        
        // In a real implementation, this would store to the actual database
        // For example: repo->create(entry);
    };
    
    // 5. Add the database sink to the lifecycle manager
    lm.add_database_sink(
        resource,
        ores::telemetry::log::make_forwarding_handler(mock_db_handler),
        "test",  // source type
        "unit-test-example"  // source name
    );
    
    // 6. Create a logger and generate some test logs
    auto lg = ores::logging::make_logger("test.component");
    
    BOOST_LOG_SEV(lg, ores::logging::boost_severity::info) 
        << "This is an info message that will be stored in the database";
    
    BOOST_LOG_SEV(lg, ores::logging::boost_severity::debug) 
        << "This is a debug message that will also be stored";
    
    BOOST_LOG_SEV(lg, ores::logging::boost_severity::error) 
        << "This is an error message for testing purposes";
    
    std::cout << "Test logs generated and sent to database sink." << std::endl;
    
    return 0;
}