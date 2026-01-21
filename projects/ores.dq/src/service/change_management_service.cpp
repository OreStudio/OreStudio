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
#include "ores.dq/service/change_management_service.hpp"

#include <stdexcept>
#include "ores.dq/domain/change_reason_category_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/change_reason_json_io.hpp" // IWYU pragma: keep.

namespace ores::dq::service {

using namespace ores::logging;

change_management_service::change_management_service(context ctx)
    : category_repo_(ctx),
      reason_repo_(ctx) {
    BOOST_LOG_SEV(lg(), info) << "Change management service initialized.";
}

// ============================================================================
// Category Management
// ============================================================================

std::vector<domain::change_reason_category>
change_management_service::list_categories() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all change reason categories.";
    return category_repo_.read_latest();
}

std::vector<domain::change_reason_category>
change_management_service::list_categories(std::uint32_t offset,
                                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing change reason categories with offset: "
                               << offset << " limit: " << limit;
    return category_repo_.read_latest(offset, limit);
}

std::uint32_t change_management_service::get_category_count() {
    BOOST_LOG_SEV(lg(), debug) << "Getting category count.";
    return category_repo_.get_total_count();
}

std::optional<domain::change_reason_category>
change_management_service::find_category(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding category by code: " << code;
    auto categories = category_repo_.read_latest(code);
    if (categories.empty()) {
        return std::nullopt;
    }
    return categories.front();
}

void change_management_service::save_category(
    const domain::change_reason_category& category) {
    if (category.code.empty()) {
        throw std::invalid_argument("Category code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving change reason category: "
                               << category.code;
    category_repo_.write(category);
    BOOST_LOG_SEV(lg(), info) << "Saved change reason category: "
                              << category.code;
}

void change_management_service::remove_category(const std::string& code) {
    BOOST_LOG_SEV(lg(), info) << "Removing change reason category: " << code;

    // Check if any reasons are using this category
    auto reasons = list_reasons_by_category(code);
    if (!reasons.empty()) {
        throw std::runtime_error("Cannot remove category '" + code +
            "' because " + std::to_string(reasons.size()) +
            " reasons are using it.");
    }

    category_repo_.remove(code);

    BOOST_LOG_SEV(lg(), info) << "Removed change reason category: " << code;
}

std::vector<domain::change_reason_category>
change_management_service::get_category_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting category history for: " << code;
    return category_repo_.read_all(code);
}

// ============================================================================
// Reason Management
// ============================================================================

std::vector<domain::change_reason>
change_management_service::list_reasons() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all change reasons.";
    return reason_repo_.read_latest();
}

std::vector<domain::change_reason>
change_management_service::list_reasons(std::uint32_t offset,
                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing change reasons with offset: "
                               << offset << " limit: " << limit;
    return reason_repo_.read_latest(offset, limit);
}

std::vector<domain::change_reason>
change_management_service::list_reasons_by_category(
    const std::string& category_code) {
    BOOST_LOG_SEV(lg(), debug) << "Listing change reasons by category: "
                               << category_code;
    return reason_repo_.read_latest_by_category(category_code);
}

std::uint32_t change_management_service::get_reason_count() {
    BOOST_LOG_SEV(lg(), debug) << "Getting reason count.";
    return reason_repo_.get_total_count();
}

std::optional<domain::change_reason>
change_management_service::find_reason(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding reason by code: " << code;
    auto reasons = reason_repo_.read_latest(code);
    if (reasons.empty()) {
        return std::nullopt;
    }
    return reasons.front();
}

void change_management_service::save_reason(
    const domain::change_reason& reason) {
    if (reason.code.empty()) {
        throw std::invalid_argument("Reason code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving change reason: " << reason.code;
    reason_repo_.write(reason);
    BOOST_LOG_SEV(lg(), info) << "Saved change reason: " << reason.code;
}

void change_management_service::remove_reason(const std::string& code) {
    BOOST_LOG_SEV(lg(), info) << "Removing change reason: " << code;

    reason_repo_.remove(code);

    BOOST_LOG_SEV(lg(), info) << "Removed change reason: " << code;
}

std::vector<domain::change_reason>
change_management_service::get_reason_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting reason history for: " << code;
    return reason_repo_.read_all(code);
}

// ============================================================================
// Validation
// ============================================================================

bool change_management_service::is_valid_reason_code(const std::string& code) {
    return find_reason(code).has_value();
}

bool change_management_service::is_valid_category_code(const std::string& code) {
    return find_category(code).has_value();
}

}
