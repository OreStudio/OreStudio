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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_field_group.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_BOND_SCHEDULE_DATES_HPP
#define ORES_TRADING_API_DOMAIN_BOND_SCHEDULE_DATES_HPP

#include <optional>
#include <string>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief A schedule the document states as an explicit date list.
 *
 * A schedule the ORE document states as a date list rather than as a rule
 * block, with the codes the list is read under. This is the date-list arm of
 * [[id:2A836172-C47A-48B6-A548-A28129C28B88][bond_schedule_data]], so the schema's choice between
 * the two spellings is mirrored as two lists and the mapping between the document and the lists
 * needs no case analysis.
 *
 * The two flags carry the document's own spelling of the schema's own
 * boolean type, as the rule arm does, so a writer re-emits the spelling the
 * reader stored.
 */
struct bond_schedule_dates {
    /**
     * @brief Business day calendar the dates are adjusted against (soft FK to
     * ores_refdata_calendars_tbl).
     */
    std::optional<std::string> calendar;

    /**
     * @brief Business day convention code the dates are adjusted under (soft FK to
     * ores_refdata_business_day_convention_types_tbl).
     */
    std::optional<std::string> convention;

    /**
     * @brief The period between dates, when the document states one.
     */
    std::optional<std::string> tenor;

    /**
     * @brief The document's own spelling of the end-of-month flag.
     */
    std::optional<std::string> end_of_month;

    /**
     * @brief The document's own spelling of the flag that keeps two dates that fall together.
     */
    std::optional<std::string> include_duplicate_dates;

    /**
     * @brief The schedule's dates, as ISO 8601 text, in document order.
     */
    std::vector<std::string> dates;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_schedule_dates&, const bond_schedule_dates&) = default;
};

}

#endif
