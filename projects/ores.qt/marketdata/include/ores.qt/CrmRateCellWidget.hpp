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
#ifndef ORES_QT_CRM_RATE_CELL_WIDGET_HPP
#define ORES_QT_CRM_RATE_CELL_WIDGET_HPP

#include <QColor>
#include <QWidget>

class QLabel;

namespace ores::qt {

/**
 * @brief Three-line cross-rates-matrix cell: pair code (small, neutral),
 * rate (bold, neutral -- never tinted, matching the reference mockup
 * where only the %-change indicator is coloured), and %-change vs. the
 * previous reload (coloured green/red/neutral).
 *
 * Kept as a real widget (not plain QTableWidgetItem text) so each line
 * can have its own font size/colour independently -- plain item text
 * can't mix styles within one cell.
 */
class CrmRateCellWidget final : public QWidget {
    Q_OBJECT

public:
    explicit CrmRateCellWidget(QWidget* parent = nullptr);

    void setDash();

    /// pairColor/pairPrefix let the pair-code line itself flag status
    /// (e.g. stale) -- always paired with a distinct glyph prefix, not
    /// colour alone, following the FxSpotGridWindow convention that a
    /// status must be visible without relying on colour perception.
    void setData(const QString& pairCode,
                 const QString& rateText,
                 const QString& changeText,
                 const QColor& changeColor,
                 const QColor& rateColor,
                 const QColor& pairColor,
                 const QString& pairPrefix = QString());

    /// Tints the whole cell with a light pastel background -- used to
    /// flag a computed reciprocal rate (1/rate, displayed because no direct
    /// quote exists for this cell) as visually distinct from a real
    /// quoted rate, without relying on the text alone.
    void setPastelBackground(bool on);

signals:
    void clicked();

protected:
    void mousePressEvent(QMouseEvent*) override;

private:
    QLabel* pairLabel_;
    QLabel* rateLabel_;
    QLabel* changeLabel_;
};

}

#endif
