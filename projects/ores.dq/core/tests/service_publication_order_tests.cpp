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
#include "ores.dq.core/service/publish_service.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using ores::dq::domain::dataset_dependency;
using ores::dq::service::publication_order;

dataset_dependency needs(std::string dataset, std::string dependency) {
    dataset_dependency d;
    d.dataset_code = std::move(dataset);
    d.dependency_code = std::move(dependency);
    return d;
}

}

TEST_CASE("datasets_with_no_dependency_between_them_keep_the_requested_order",
          "[publication_order]") {
    const std::vector<std::string> requested{
        "iso.currencies", "synthetic.feeds.lseg", "ore.parameters", "ore.conventions"};

    CHECK(publication_order(requested, {}) == requested);
}

TEST_CASE("a_dependency_requested_later_moves_up_to_just_before_what_needs_it",
          "[publication_order]") {
    const auto order = publication_order({"refdata.currency_pairs", "other", "iso.currencies"},
                                         {needs("refdata.currency_pairs", "iso.currencies")});

    CHECK(order == std::vector<std::string>{"iso.currencies", "refdata.currency_pairs", "other"});
}

TEST_CASE("an_unrequested_dependency_publishes_just_before_what_needs_it", "[publication_order]") {
    const auto order = publication_order({"first", "second", "third"},
                                         {needs("third", "pulled"), needs("pulled", "deeper")});

    CHECK(order == std::vector<std::string>{"first", "second", "deeper", "pulled", "third"});
}

TEST_CASE("a_dependency_shared_by_two_datasets_publishes_once", "[publication_order]") {
    const auto order = publication_order(
        {"a", "b"}, {needs("a", "shared"), needs("b", "shared"), needs("a", "shared")});

    CHECK(order == std::vector<std::string>{"shared", "a", "b"});
}

TEST_CASE("dependencies_unrelated_to_the_request_are_left_out", "[publication_order]") {
    CHECK(publication_order({"a"}, {needs("elsewhere", "a"), needs("other", "unrelated")}) ==
          std::vector<std::string>{"a"});
}

TEST_CASE("a_cycle_of_dependencies_is_refused", "[publication_order]") {
    CHECK_THROWS_AS(publication_order({"a", "b"}, {needs("a", "b"), needs("b", "a")}),
                    std::runtime_error);
}
