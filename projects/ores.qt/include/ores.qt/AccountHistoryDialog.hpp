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
#ifndef ORES_QT_ACCOUNT_HISTORY_DIALOG_HPP
#define ORES_QT_ACCOUNT_HISTORY_DIALOG_HPP

#include <memory>
#include <QPair>
#include <QWidget>
#include <QString>
#include <QVector>
#include "ores.qt/ClientManager.hpp"
#include "ores.accounts/domain/account_version.hpp"
#include "ores.accounts/domain/account_version_history.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ui_AccountHistoryDialog.h"

namespace Ui {
class AccountHistoryDialog;
}

namespace ores::qt {

/**
 * @brief Widget for displaying account version history.
 */
class AccountHistoryDialog : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.account_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    const QIcon& getHistoryIcon() const;

public:
    explicit AccountHistoryDialog(QString username,
        ClientManager* clientManager,
        QWidget* parent = nullptr);
    ~AccountHistoryDialog() override;

    void loadHistory();

    QSize sizeHint() const override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private slots:
    void onVersionSelected(int index);
    void onHistoryLoaded();
    void onHistoryLoadError(const QString& error);

private:
    void displayChangesTab(int version_index);
    void displayFullDetailsTab(int version_index);

    /**
     * @brief Calculate differences between two versions.
     *
     * @return Vector of (field_name, (old_value, new_value)) pairs.
     */
    using DiffResult = QVector<QPair<QString, QPair<QString, QString>>>;
    DiffResult calculateDiff(
        const accounts::domain::account_version& current,
        const accounts::domain::account_version& previous);

    std::unique_ptr<Ui::AccountHistoryDialog> ui_;
    ClientManager* clientManager_;
    QString username_;
    accounts::domain::account_version_history history_;
};

}

#endif
