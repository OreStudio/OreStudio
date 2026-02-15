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
#ifndef ORES_QT_PASSWORD_MATCH_INDICATOR_HPP
#define ORES_QT_PASSWORD_MATCH_INDICATOR_HPP

#include <QString>
#include <QLineEdit>
#include <QObject>

namespace ores::qt {

/**
 * @brief Helper for password confirmation match indicators.
 *
 * Changes the confirm field's border colour to green when passwords match
 * or red when they don't. Use @c connectFields to wire up the indicator
 * automatically, or call @c updateFieldBorder manually.
 */
struct PasswordMatchIndicator {
    static inline const QString match_border_style =
        QStringLiteral("QLineEdit { border: 2px solid #4CAF50; }");
    static inline const QString mismatch_border_style =
        QStringLiteral("QLineEdit { border: 2px solid #cc0000; }");

    /**
     * @brief Update the border colour of the confirm password field.
     *
     * Resets to the default style when confirm is empty, green border when
     * passwords match, and red border when they don't.
     */
    static void updateFieldBorder(QLineEdit* field,
        const QString& password, const QString& confirm,
        const QString& defaultStyle = {}) {
        if (confirm.isEmpty()) {
            field->setStyleSheet(defaultStyle);
        } else if (password == confirm) {
            field->setStyleSheet(match_border_style);
        } else {
            field->setStyleSheet(mismatch_border_style);
        }
    }

    /**
     * @brief Connect two password fields so the confirm field's border
     * updates automatically on every keystroke.
     */
    static void connectFields(QLineEdit* passwordField,
        QLineEdit* confirmField,
        const QString& defaultStyle = {}) {
        auto update = [=]() {
            updateFieldBorder(confirmField, passwordField->text(),
                confirmField->text(), defaultStyle);
        };
        QObject::connect(passwordField, &QLineEdit::textChanged,
            confirmField, update);
        QObject::connect(confirmField, &QLineEdit::textChanged,
            confirmField, update);
    }
};

}

#endif
