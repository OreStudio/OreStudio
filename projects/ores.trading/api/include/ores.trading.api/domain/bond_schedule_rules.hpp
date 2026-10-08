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
#ifndef ORES_TRADING_API_DOMAIN_BOND_SCHEDULE_RULES_HPP
#define ORES_TRADING_API_DOMAIN_BOND_SCHEDULE_RULES_HPP

#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief A schedule the document states as a tenor expanded against a calendar.
 *
 * A schedule the ORE document states as a rule block rather than as a date
 * list: a start date and a tenor, expanded against a calendar under a date
 * rule. The generated code mirrors the schema's choice between the two
 * spellings as two lists on bond_schedule_data, and this type is the rule
 * arm, so the mapping between the document and the two lists needs no case
 * analysis.
 *
 * The codes (calendar, convention, term convention, rule and the two
 * end-of-month members) are the canonical spellings the ORE schema uses. A
 * member the schema declares optional is an optional here, so an element the
 * document states empty stays distinct from one it omits.
 *
 * Two members are flags the schema types as its own boolean rather than as
 * the XML boolean. That type enumerates thirteen spellings, including the
 * empty one, and the writer emits the spelling the reader stored, so the two
 * carry the document's text: the corpus writes these elements empty for the
 * on state. The two remove flags are XML booleans and stay boolean.
 *
 * The nine bond tables carry no schedule column. The exporter re-emits the
 * document's own statement, so the container holds this whole rather than
 * projecting it onto a table that does not exist.
 */
struct bond_schedule_rules {
    /**
     * @brief The date the schedule starts from, as ISO 8601 text.
     */
    std::string start_date;

    /**
     * @brief The date the schedule ends on, when the document states one.
     */
    std::optional<std::string> end_date;

    /**
     * @brief The document's own spelling of the flag that pulls the end date back to the previous
     * month end.
     */
    std::optional<std::string> adjust_end_date_to_previous_month_end;

    /**
     * @brief The period the schedule steps by, in ORE's tenor spelling.
     */
    std::string tenor;

    /**
     * @brief Business day calendar the dates are adjusted against (soft FK to
     * ores_refdata_calendars_tbl).
     */
    std::optional<std::string> calendar;

    /**
     * @brief Business day convention code the dates are adjusted under (soft FK to
     * ores_refdata_business_day_convention_types_tbl).
     */
    std::string convention;

    /**
     * @brief Convention applied at the schedule's end, when the document states one.
     */
    std::optional<std::string> term_convention;

    /**
     * @brief Date rule the tenor is expanded under, such as Forward or Backward.
     */
    std::optional<std::string> rule;

    /**
     * @brief The document's own spelling of the end-of-month flag.
     */
    std::optional<std::string> end_of_month;

    /**
     * @brief Convention applied when the end-of-month flag is on.
     */
    std::optional<std::string> end_of_month_convention;

    /**
     * @brief The schedule's first date, when the document states it outright.
     */
    std::optional<std::string> first_date;

    /**
     * @brief The schedule's last date, when the document states it outright.
     */
    std::optional<std::string> last_date;

    /**
     * @brief True when the document asks for the first date to be dropped.
     */
    std::optional<bool> remove_first_date;

    /**
     * @brief True when the document asks for the last date to be dropped.
     */
    std::optional<bool> remove_last_date;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_schedule_rules&, const bond_schedule_rules&) = default;
};

}

#endif
