/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_MESSAGE_BOX_HELPER_HPP
#define ORES_QT_MESSAGE_BOX_HELPER_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <QMessageBox>
#include <QWidget>
#include <QString>
#include <QIcon>

namespace ores::qt {

class MessageBoxHelper {
public:
    /**
     * @brief Show a question dialog with custom Fluent icons
     * @param parent Parent widget
     * @param title Dialog title
     * @param text Dialog message
     * @param buttons Buttons to show (default Yes | No)
     * @return The button that was clicked
     */
    static QMessageBox::StandardButton question(
        QWidget* parent,
        const QString& title,
        const QString& text,
        QMessageBox::StandardButtons buttons = QMessageBox::Yes | QMessageBox::No);

    /**
     * @brief Show a warning dialog with custom Fluent icon
     * @param parent Parent widget
     * @param title Dialog title
     * @param text Dialog message
     */
    static void warning(
        QWidget* parent,
        const QString& title,
        const QString& text);

    /**
     * @brief Show a critical error dialog with custom Fluent icon
     * @param parent Parent widget
     * @param title Dialog title
     * @param text Dialog message
     */
    static void critical(
        QWidget* parent,
        const QString& title,
        const QString& text);

    /**
     * @brief Show an information dialog with custom Fluent icon
     * @param parent Parent widget
     * @param title Dialog title
     * @param text Dialog message
     */
    static void information(
        QWidget* parent,
        const QString& title,
        const QString& text);

private:
    static QIcon createColoredIcon(const QString& svgPath, const QColor& color);
};

} // namespace ores::qt

#endif // ORES_QT_MESSAGE_BOX_HELPER_HPP
