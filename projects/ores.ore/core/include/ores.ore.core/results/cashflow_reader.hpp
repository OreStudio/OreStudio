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
#ifndef ORES_ORE_CORE_RESULTS_CASHFLOW_READER_HPP
#define ORES_ORE_CORE_RESULTS_CASHFLOW_READER_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::results {

/**
 * @brief One figure of one cashflow a run produced.
 *
 * A cashflow is not a metric. Its row states which payment it is, by leg and
 * number, and the stressed families state each of its figures three times: as a
 * base, as a scenario and as the difference between them.
 */
struct cashflow_row {
    /**
     * @brief The kind of record the cashflow belongs to: a trade or a market object.
     */
    std::string subject_type;
    /**
     * @brief The engine's own identifier for that record.
     */
    std::string subject;
    /**
     * @brief The scenario the cashflow was stressed under, empty when the file states none.
     */
    std::string scenario;
    /**
     * @brief The observation date the cashflow belongs to, empty when the row states none.
     */
    std::string as_of;
    /**
     * @brief The engine's number for the payment within its leg.
     */
    std::string cashflow_no;
    /**
     * @brief The engine's number for the leg the payment belongs to.
     */
    std::string leg_no;
    /**
     * @brief The day the payment falls due, as the engine wrote it.
     */
    std::string pay_date;
    /**
     * @brief The engine's name for the kind of payment, such as Interest.
     */
    std::string flow_type;
    /**
     * @brief The currency the cashflow is stated in, empty when it states none.
     */
    std::string currency;
    /**
     * @brief The engine's name for the figure, which is its heading.
     */
    std::string measure;
    /**
     * @brief Which part of a stressed comparison the figure is: base, scen or diff.
     *
     * A family that states one figure for each payment leaves it empty.
     */
    std::string part;
    /**
     * @brief The context of the figure, in the order the file states it.
     */
    std::vector<std::pair<std::string, std::string>> qualifiers;
    /**
     * @brief The figure as the engine stated it.
     */
    double value = 0.0;
    /**
     * @brief The file the cashflow came from.
     */
    std::string source;
};

/**
 * @brief What reading a family of cashflows produced, and what it refused.
 *
 * A refusal means the run's cashflows must not be loaded, because a partial load
 * reads as a complete one.
 */
struct cashflow_read_outcome {
    /**
     * @brief The figures the file stated.
     */
    std::vector<cashflow_row> rows;
    /**
     * @brief The reasons the file could not be read whole.
     */
    std::vector<std::string> refusals;
};

/**
 * @brief Whether a family's row states a payment rather than a subject.
 *
 * A cashflow family is one whose key columns name a payment: the leg and the
 * cashflow number. Every such family is read by @ref read_cashflows, and no
 * figure of it is stored as a metric.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT bool is_cashflow_family(const output_family& family);

/**
 * @brief Reads a family whose row is a payment with figures.
 *
 * The keys that name the payment are carried as the row's own columns; every
 * other key and qualifier travels beside the figure. Each measure column becomes
 * one row named by its heading, and a heading the stressed families state as
 * =Amount_Base=, =Amount_Scen= or =Amount_Diff= becomes one row named =Amount=
 * with the part that says which of the three it is, so a reader compares the
 * three without learning the suffix.
 *
 * A row that states no leg or no cashflow number is refused: without them the
 * figures cannot be told apart from another payment's. A figure the engine
 * states as its null string is not a row, and is not a refusal.
 *
 * @param csv     the file's bytes, header included.
 * @param family  the family the catalogue gives the file.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT cashflow_read_outcome
read_cashflows(std::string_view csv, const output_family& family);

/**
 * @brief Reads a file by its name, asking the catalogue which family it is.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT cashflow_read_outcome
read_cashflows_for_file(std::string_view csv, std::string_view file_name);

}

#endif
