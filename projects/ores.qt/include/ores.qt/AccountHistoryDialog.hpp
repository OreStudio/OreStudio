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
#include <QToolBar>
#include <QAction>
#include "ores.qt/ClientManager.hpp"
#include "ores.iam/domain/account_version.hpp"
#include "ores.iam/domain/account_version_history.hpp"
#include "ores.logging/make_logger.hpp"
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
        using namespace ores::logging;
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

    /**
     * @brief Mark the history data as stale and reload.
     *
     * Called when a notification is received indicating this account has
     * changed on the server. Automatically reloads the history data.
     */
    void markAsStale();

    /**
     * @brief Returns the username of the account.
     */
    [[nodiscard]] QString username() const { return username_; }

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

    /**
     * @brief Emitted when user requests to open a version in read-only mode.
     * @param account The account data at the selected version.
     * @param versionNumber The version number being viewed.
     */
    void openVersionRequested(const iam::domain::account& account, int versionNumber);

    /**
     * @brief Emitted when user requests to revert to a selected version.
     * @param account The account data to revert to.
     */
    void revertVersionRequested(const iam::domain::account& account);

private slots:
    void onVersionSelected(int index);
    void onHistoryLoaded();
    void onHistoryLoadError(const QString& error);
    void onOpenClicked();
    void onRevertClicked();
    void onReloadClicked();

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
        const iam::domain::account_version& current,
        const iam::domain::account_version& previous);

    void setupToolbar();
    void updateButtonStates();
    int selectedVersionIndex() const;

    std::unique_ptr<Ui::AccountHistoryDialog> ui_;
    ClientManager* clientManager_;
    QString username_;
    iam::domain::account_version_history history_;

    QToolBar* toolBar_;
    QAction* reloadAction_;
    QAction* openAction_;
    QAction* revertAction_;
};

}

#endif
