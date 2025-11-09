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
#ifndef ORES_QT_CURRENCY_HISTORY_DIALOG_HPP
#define ORES_QT_CURRENCY_HISTORY_DIALOG_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <QDialog>
#include <QString>
#include <QVector>
#include <QPair>
#include <memory>
#include "ores.comms/client.hpp"
#include "ores.risk/domain/currency_version.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ui_CurrencyHistoryDialog.h"

namespace Ui {
class CurrencyHistoryDialog;
}

namespace ores::qt {

/**
 * @brief Dialog for displaying currency version history.
 */
class CurrencyHistoryDialog : public QDialog {
    Q_OBJECT

private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.currency_history_dialog");
        return instance;
    }

public:
    explicit CurrencyHistoryDialog(const QString& iso_code,
                                   std::shared_ptr<comms::client> client,
                                   QWidget* parent = nullptr);
    ~CurrencyHistoryDialog() override;

private slots:
    void onVersionSelected(int index);
    void onHistoryLoaded();
    void onHistoryLoadError(const QString& error);

private:
    void loadHistory();
    void displayChangesTab(int version_index);
    void displayFullDetailsTab(int version_index);

    /**
     * @brief Calculate differences between two versions.
     * @return List of (field_name, (old_value, new_value)) pairs
     */
    QVector<QPair<QString, QPair<QString, QString>>> calculateDiff(
        const risk::domain::currency_version& current,
        const risk::domain::currency_version& previous);

    Ui::CurrencyHistoryDialog* ui_;
    std::shared_ptr<comms::client> client_;
    QString isoCode_;
    risk::domain::currency_version_history history_;
};

}

#endif
