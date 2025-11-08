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
#ifndef ORES_QT_CURRENCY_EDIT_DIALOG_HPP
#define ORES_QT_CURRENCY_EDIT_DIALOG_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <QDialog>
#include <memory>
#include "ores.comms/client.hpp"
#include "ores.risk/domain/currency.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace Ui {
class CurrencyEditDialog;
}

namespace ores::qt {

/**
 * @brief Dialog for editing currency details.
 */
class CurrencyEditDialog : public QDialog {
    Q_OBJECT

private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.currency_edit_dialog");
        return instance;
    }

public:
    explicit CurrencyEditDialog(const ores::risk::domain::currency& currency,
                                std::shared_ptr<comms::client> client,
                                QWidget* parent = nullptr);
    ~CurrencyEditDialog() override;

signals:
    void currencyUpdated();
    void currencyDeleted(const QString& iso_code);

private slots:
    void onSaveClicked();
    void onDeleteClicked();
    void onResetClicked();
    void onFieldChanged();

private:
    void populateFields();
    void resetFields();
    bool validateFields();
    void updateSaveButtonState();
    bool hasChanges() const;

private:
    Ui::CurrencyEditDialog* ui_;
    ores::risk::domain::currency original_;
    std::shared_ptr<comms::client> client_;
    bool has_changes_;
};

}

#endif
