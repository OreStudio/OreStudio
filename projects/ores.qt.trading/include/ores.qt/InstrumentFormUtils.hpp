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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_INSTRUMENT_FORM_UTILS_HPP
#define ORES_QT_INSTRUMENT_FORM_UTILS_HPP

#include <string>

class QComboBox;

namespace ores::qt {

/**
 * @brief Static helpers for populating and reading instrument-form combo boxes.
 *
 * Each populate* method fills a QComboBox with the fixed vocabulary for that
 * field. The first item is always an empty string so that optional fields can
 * be left unset. All values match the canonical strings accepted by ORE/NATS
 * handlers and stored in the database.
 */
class InstrumentFormUtils {
public:
    InstrumentFormUtils() = delete;

    // ── Field-specific populators ──────────────────────────────────────────────

    /** "", "Cash", "Physical" */
    static void populateSettlement(QComboBox* cb);

    /** "", "Call", "Put" */
    static void populateOptionType(QComboBox* cb);

    /** "", "European", "American" — FX vanilla options only (no Bermudan) */
    static void populateExerciseStyle(QComboBox* cb);

    /** "", "European", "American", "Bermudan" — equity / commodity */
    static void populateExerciseType(QComboBox* cb);

    /** "", A360, A365F, A365, ActAct(ISDA), ActAct(ISMA), ActAct(AFB),
     *  30/360, 30E/360, 30E/360(ISDA), Business252, 1/1, Simple */
    static void populateDayCount(QComboBox* cb);

    /** "", Annual, Semiannual, EveryFourthMonth, Quarterly, Bimonthly,
     *  Monthly, EveryFourthWeek, Biweekly, Weekly, Daily, Once */
    static void populateFrequency(QComboBox* cb);

    /** "", "UpIn", "UpOut", "DownIn", "DownOut" */
    static void populateBarrierType(QComboBox* cb);

    /** "", "Arithmetic", "Geometric" */
    static void populateAverageType(QComboBox* cb);

    /** "", "Senior", "Subordinated" */
    static void populateSeniority(QComboBox* cb);

    /** "", "MM", "MR", "CR", "XR" */
    static void populateRestructuring(QComboBox* cb);

    /** "", "TotalReturn", "PriceReturn" */
    static void populateTrsReturnType(QComboBox* cb);

    /** "", "Call", "Put" — ASCOT bond option type */
    static void populateAscotOptionType(QComboBox* cb);

    /** "", "TotalReturn", "PriceReturn" — equity swap / TRS return type */
    static void populateReturnType(QComboBox* cb);

    // ── Value accessors ────────────────────────────────────────────────────────

    /**
     * @brief Select the combo item whose text equals @p value.
     *        Falls back to index 0 (the empty entry) if not found.
     */
    static void setComboValue(QComboBox* cb, const std::string& value);

    /**
     * @brief Return the currently selected item text as a std::string.
     */
    static std::string getComboValue(const QComboBox* cb);
};

} // namespace ores::qt

#endif
