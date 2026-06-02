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
#include "ores.qt/OreDateEdit.hpp"

OreDateEdit::OreDateEdit(QWidget* parent)
    : QDateEdit(parent) {
    setCalendarPopup(true);
    setDisplayFormat(QStringLiteral("yyyy-MM-dd"));
    setMinimumDate(QDate(1900, 1, 1));
    // A single space signals the blank/unset state visually.
    setSpecialValueText(QStringLiteral(" "));
    setDate(minimumDate());
}

std::string OreDateEdit::isoDate() const {
    if (date() == minimumDate())
        return {};
    return date().toString(QStringLiteral("yyyy-MM-dd")).toStdString();
}

void OreDateEdit::setIsoDate(const std::string& iso) {
    if (iso.empty()) {
        setDate(minimumDate());
        return;
    }
    const QDate d = QDate::fromString(
        QString::fromStdString(iso), QStringLiteral("yyyy-MM-dd"));
    setDate(d.isValid() ? d : minimumDate());
}
