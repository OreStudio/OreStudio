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
#ifndef ORES_QT_DIALOG_STYLES_HPP
#define ORES_QT_DIALOG_STYLES_HPP

#include <QString>

namespace ores::qt {

/**
 * @brief Centralized stylesheet constants for dark-themed dialogs.
 *
 * These styles are shared between LoginDialog and SignUpDialog to ensure
 * visual consistency. Change them here to update the appearance globally.
 */
struct dialog_styles {
    static inline const QString panel = R"(
        QWidget#mainPanel {
            background-color: #1A1A1A;
        }
    )";

    static inline const QString input_field = R"(
        QLineEdit {
            background-color: #2d2d2d;
            border: 1px solid #3d3d3d;
            border-radius: 4px;
            padding: 8px 12px;
            font-size: 13px;
            color: #ffffff;
        }
        QLineEdit:focus {
            border-color: #5a5a5a;
            background-color: #333333;
        }
        QLineEdit::placeholder {
            color: #707070;
        }
    )";

    static inline const QString input_field_match = R"(
        QLineEdit {
            background-color: #2d2d2d;
            border: 2px solid #4CAF50;
            border-radius: 4px;
            padding: 8px 12px;
            font-size: 13px;
            color: #ffffff;
        }
        QLineEdit:focus {
            border-color: #4CAF50;
            background-color: #333333;
        }
    )";

    static inline const QString input_field_mismatch = R"(
        QLineEdit {
            background-color: #2d2d2d;
            border: 2px solid #FF9800;
            border-radius: 4px;
            padding: 8px 12px;
            font-size: 13px;
            color: #ffffff;
        }
        QLineEdit:focus {
            border-color: #FF9800;
            background-color: #333333;
        }
    )";

    static inline const QString spin_box = R"(
        QSpinBox {
            background-color: #2d2d2d;
            border: 1px solid #3d3d3d;
            border-radius: 4px;
            padding: 8px 12px;
            font-size: 13px;
            color: #ffffff;
        }
        QSpinBox:focus {
            border-color: #5a5a5a;
            background-color: #333333;
        }
    )";

    static inline const QString primary_button = R"(
        QPushButton {
            background-color: #3d3d3d;
            color: #ffffff;
            border: none;
            border-radius: 4px;
            padding: 10px 24px;
            font-size: 14px;
            font-weight: bold;
        }
        QPushButton:hover {
            background-color: #4a4a4a;
        }
        QPushButton:pressed {
            background-color: #333333;
        }
        QPushButton:disabled {
            background-color: #2a2a2a;
            color: #555555;
        }
    )";

    static inline const QString checkbox = R"(
        QCheckBox {
            background: transparent;
            color: #909090;
            font-size: 12px;
            spacing: 6px;
        }
        QCheckBox::indicator {
            width: 14px;
            height: 14px;
            border: 1px solid #3d3d3d;
            border-radius: 2px;
            background-color: #2d2d2d;
        }
        QCheckBox::indicator:checked {
            background-color: #4a4a4a;
            border-color: #5a5a5a;
        }
    )";

    static inline const QString link_button = R"(
        QPushButton {
            background: transparent;
            border: none;
            color: #909090;
            font-size: 12px;
            padding: 0;
        }
        QPushButton:hover {
            color: #ffffff;
            text-decoration: underline;
        }
    )";

    static inline const QString field_label = R"(
        QLabel {
            background: transparent;
            color: #909090;
            font-size: 10px;
            font-weight: bold;
            letter-spacing: 1px;
        }
    )";

    static inline const QString version = R"(
        QLabel {
            background: transparent;
            color: #505050;
            font-size: 9px;
        }
    )";

    static inline const QString status = R"(
        QLabel {
            background: transparent;
            color: #707070;
            font-size: 11px;
            font-style: italic;
        }
    )";

    static inline const QString subtitle = R"(
        QLabel {
            background: transparent;
            color: #707070;
            font-size: 12px;
        }
    )";

    static inline const QString saved_connections_button = R"(
        QToolButton {
            background: transparent;
            border: none;
            padding: 2px;
        }
        QToolButton:hover {
            background-color: #2d2d2d;
            border-radius: 2px;
        }
        QToolButton::menu-indicator {
            image: none;
        }
    )";
};

}

#endif
