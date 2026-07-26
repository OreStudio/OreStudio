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
#include "ores.qt/ColourSwatchHelper.hpp"
#include <QColor>
#include <QColorDialog>
#include <QSizePolicy>

namespace ores::qt {

void set_colour_swatch(QPushButton* button, const QString& hex) {
    if (!button)
        return;

    QColor colour(hex);
    if (!colour.isValid())
        colour = QColor(Qt::white);

    const QString normalised = colour.name();
    const QColor label = colour.lightnessF() > 0.5 ? QColor(Qt::black) : QColor(Qt::white);

    button->setText(normalised);
    button->setStyleSheet(QString("background-color: %1; color: %2; border: none; "
                                   "border-radius: 10px; padding: 2px 10px; font-weight: bold;")
                              .arg(normalised, label.name()));
    button->setProperty("hexColour", normalised);

    // QPushButton's default horizontal size policy is Minimum, so it
    // stretches to fill the form column like the QLineEdits beside it.
    // The badge should stay pill-sized, matching how it renders in lists.
    button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    button->adjustSize();
}

QString colour_swatch_value(const QPushButton* button) {
    if (!button)
        return QStringLiteral("#ffffff");

    const auto stored = button->property("hexColour").toString();
    return stored.isEmpty() ? QStringLiteral("#ffffff") : stored;
}

void wire_colour_picker(QObject* context, QPushButton* button, const std::function<void()>& on_changed) {
    if (!button)
        return;

    QObject::connect(button, &QPushButton::clicked, context, [button, on_changed]() {
        const QColor initial(colour_swatch_value(button));
        const QColor chosen =
            QColorDialog::getColor(initial, button->window(), QObject::tr("Choose Colour"));
        if (!chosen.isValid())
            return;

        set_colour_swatch(button, chosen.name());
        if (on_changed)
            on_changed();
    });
}

}
