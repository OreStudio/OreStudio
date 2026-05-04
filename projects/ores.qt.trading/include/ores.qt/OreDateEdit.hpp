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
#ifndef ORES_QT_ORE_DATE_EDIT_HPP
#define ORES_QT_ORE_DATE_EDIT_HPP

#include <string>
#include <QDateEdit>

class OreDateEdit : public QDateEdit {
public:
    explicit OreDateEdit(QWidget* parent = nullptr);

    /**
     * @brief Returns the selected date as "yyyy-MM-dd", or an empty string
     *        when the widget is in the blank (unset) state.
     */
    std::string isoDate() const;

    /**
     * @brief Sets the widget to the date represented by @p iso ("yyyy-MM-dd").
     *        An empty string resets the widget to the blank (unset) state.
     */
    void setIsoDate(const std::string& iso);
};

#endif
