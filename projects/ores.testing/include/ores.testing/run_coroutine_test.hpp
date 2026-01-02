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
#ifndef ORES_TESTING_DATABASE_RUN_COROUTINE_TEST_HPP
#define ORES_TESTING_DATABASE_RUN_COROUTINE_TEST_HPP

#include <exception>
#include <string>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>

namespace ores::testing {

template <typename Awaitable>
void run_coroutine_test(boost::asio::io_context& io_context, Awaitable&& awaitable) {
    bool completed = false;
    std::exception_ptr eptr = nullptr;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
            try {
                co_await std::forward<Awaitable>(awaitable)();
                completed = true;
            } catch (...) {
                eptr = std::current_exception();
            }
        }, boost::asio::detached);
    io_context.run();

    if (eptr) {
        try {
            std::rethrow_exception(eptr);
        } catch (const std::exception& e) {
            FAIL("Coroutine threw exception: " << e.what());
        } catch (...) {
            FAIL("Coroutine threw unknown exception");
        }
    }

    REQUIRE(completed);
    io_context.restart(); // Prepare for next use if in a loop
}

}


#endif
