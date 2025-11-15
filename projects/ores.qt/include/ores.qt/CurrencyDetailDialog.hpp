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
#ifndef ORES_QT_CURRENCY_DETAIL_DIALOG_HPP
#define ORES_QT_CURRENCY_DETAIL_DIALOG_HPP

#include <QWidget>
#include <QToolBar>
#include <QAction>
#include <memory>
#include "ores.risk/domain/currency.hpp"
#include "ores.comms/net/client.hpp"
#include "ores.utility/log/make_logger.hpp"


namespace Ui {

class CurrencyDetailDialog;

}

namespace ores::qt {

class CurrencyDetailDialog final : public QWidget {
    Q_OBJECT

private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.currency_detail_dialog");
        return instance;
    }

public:
    explicit CurrencyDetailDialog(QWidget* parent = nullptr);
    ~CurrencyDetailDialog() override;

    void setClient(std::shared_ptr<comms::client> client);
    void setUsername(const std::string& username);

    void setCurrency(const risk::domain::currency& currency);
    [[nodiscard]] risk::domain::currency getCurrency() const;
    void clearDialog();
    void save();

signals:
    void currencyUpdated(const QString& iso_code);
    void currencyCreated(const QString& iso_code);
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
    void updateSaveResetButtonState();

private:
    std::unique_ptr<Ui::CurrencyDetailDialog> ui_;
    bool isDirty_;
    bool isAddMode_;
    std::string username_;
    QToolBar* toolBar_;
    QAction* saveAction_;
    QAction* deleteAction_;

    std::shared_ptr<comms::client> client_;
    risk::domain::currency currentCurrency_;
    static constexpr const char* max_timestamp = "9999-12-31 23:59:59";
};

}

#endif
