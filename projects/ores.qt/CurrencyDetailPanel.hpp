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
#ifndef ORES_QT_CURRENCY_DETAIL_PANEL_HPP
#define ORES_QT_CURRENCY_DETAIL_PANEL_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <QWidget>
#include <memory>
#include "ores.risk/domain/currency.hpp"
#include "ores.comms/client.hpp"

namespace Ui {
class CurrencyDetailPanel;
}

namespace ores::qt {

class CurrencyDetailPanel : public QWidget {
    Q_OBJECT

public:
    explicit CurrencyDetailPanel(QWidget* parent = nullptr); // Removed client from constructor
    ~CurrencyDetailPanel() override;

    void setClient(std::shared_ptr<comms::client> client); // New method to set client

    void setCurrency(const risk::domain::currency& currency);
    risk::domain::currency getCurrency() const;
    void clearPanel();
    void save();

signals:
    void currencyUpdated();
    void currencyDeleted(const QString& iso_code);
    void statusMessage(const QString& message);
    void errorMessage(const QString& message);
    void isDirtyChanged(bool isDirty);

private slots:
    void onSaveClicked();
    void onResetClicked();
    void onDeleteClicked();
    void onFieldChanged();

private:
    std::unique_ptr<Ui::CurrencyDetailPanel> ui_;
    std::shared_ptr<comms::client> client_;
    risk::domain::currency currentCurrency_;
    bool isDirty_;

    void updateSaveResetButtonState();
};

} // namespace ores::qt

#endif // ORES_QT_CURRENCY_DETAIL_PANEL_HPP
