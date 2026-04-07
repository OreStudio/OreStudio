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
#ifndef ORES_QT_COMPOSITE_LEGS_WIDGET_HPP
#define ORES_QT_COMPOSITE_LEGS_WIDGET_HPP

#include <vector>
#include <QWidget>
#include "ores.trading.api/domain/composite_leg.hpp"

namespace Ui {
class CompositeLegsWidget;
}

namespace ores::qt {

/**
 * @brief Reusable widget for displaying and editing a list of composite legs.
 *
 * Manages a QTableWidget with Seq / Trade ID columns. Embed in any dialog
 * that deals with composite_leg records (CompositeTrade, MultiLegOption).
 */
class CompositeLegsWidget final : public QWidget {
    Q_OBJECT

public:
    explicit CompositeLegsWidget(QWidget* parent = nullptr);
    ~CompositeLegsWidget() override;

    /**
     * @brief Populate the table from a vector of legs.
     *
     * Does not emit legsChanged().
     */
    void setLegs(const std::vector<trading::domain::composite_leg>& legs);

    /**
     * @brief Collect the current table contents as a typed vector.
     *
     * Rows with an empty Trade ID are skipped.
     */
    std::vector<trading::domain::composite_leg> legs() const;

    /**
     * @brief Enable or disable editing of the legs table and buttons.
     */
    void setReadOnly(bool readOnly);

signals:
    void legsChanged();

private slots:
    void onAddLegClicked();
    void onRemoveLegClicked();

private:
    void setupUi();
    void setupConnections();

    Ui::CompositeLegsWidget* ui_;
    bool readOnly_{false};
};

}

#endif
