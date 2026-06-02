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
#include "ores.qt/InstrumentFormUtils.hpp"

#include <QComboBox>

namespace ores::qt {

void InstrumentFormUtils::populateSettlement(QComboBox* cb) {
    cb->clear();
    cb->addItems({"", "Cash", "Physical"});
}

void InstrumentFormUtils::populateOptionType(QComboBox* cb) {
    cb->clear();
    cb->addItems({"", "Call", "Put"});
}

void InstrumentFormUtils::populateExerciseStyle(QComboBox* cb) {
    cb->clear();
    cb->addItems({"", "European", "American"});
}

void InstrumentFormUtils::populateExerciseType(QComboBox* cb) {
    cb->clear();
    cb->addItems({"", "European", "American", "Bermudan"});
}

void InstrumentFormUtils::populateDayCount(QComboBox* cb) {
    cb->clear();
    cb->addItems({
        "",
        "A360",
        "A365F",
        "A365",
        "ActAct(ISDA)",
        "ActAct(ISMA)",
        "ActAct(AFB)",
        "30/360",
        "30E/360",
        "30E/360(ISDA)",
        "Business252",
        "1/1",
        "Simple",
    });
}

void InstrumentFormUtils::populateFrequency(QComboBox* cb) {
    cb->clear();
    cb->addItems({
        "",
        "Annual",
        "Semiannual",
        "EveryFourthMonth",
        "Quarterly",
        "Bimonthly",
        "Monthly",
        "EveryFourthWeek",
        "Biweekly",
        "Weekly",
        "Daily",
        "Once",
    });
}

void InstrumentFormUtils::populateBarrierType(QComboBox* cb) {
    cb->clear();
    cb->addItems({
        "",
        "UpAndIn",
        "UpAndOut",
        "DownAndIn",
        "DownAndOut",
        "KnockIn",
        "KnockOut",
    });
}

void InstrumentFormUtils::populateAverageType(QComboBox* cb) {
    cb->clear();
    cb->addItems({"", "Arithmetic", "Geometric"});
}

void InstrumentFormUtils::populateSeniority(QComboBox* cb) {
    cb->clear();
    cb->addItems({"", "Senior", "Subordinated"});
}

void InstrumentFormUtils::populateRestructuring(QComboBox* cb) {
    cb->clear();
    cb->addItems({"", "MM", "MR", "CR", "XR"});
}

void InstrumentFormUtils::populateTrsReturnType(QComboBox* cb) {
    cb->clear();
    cb->addItems({"", "TotalReturn", "PriceReturn"});
}

void InstrumentFormUtils::populateAscotOptionType(QComboBox* cb) {
    cb->clear();
    cb->addItems({"", "Call", "Put"});
}

void InstrumentFormUtils::populateReturnType(QComboBox* cb) {
    cb->clear();
    cb->addItems({"", "TotalReturn", "PriceReturn"});
}

void InstrumentFormUtils::populateLongShort(QComboBox* cb) {
    cb->clear();
    cb->addItems({"", "Long", "Short"});
}

void InstrumentFormUtils::populateMomentType(QComboBox* cb) {
    cb->clear();
    cb->addItems({"", "Variance", "Volatility"});
}

void InstrumentFormUtils::setComboValue(QComboBox* cb, const std::string& value) {
    const QString qv = QString::fromStdString(value);
    const int idx = cb->findText(qv);
    cb->setCurrentIndex(idx >= 0 ? idx : 0);
}

std::string InstrumentFormUtils::getComboValue(const QComboBox* cb) {
    return cb->currentText().toStdString();
}

} // namespace ores::qt
