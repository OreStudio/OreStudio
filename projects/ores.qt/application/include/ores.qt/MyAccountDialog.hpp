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
#ifndef ORES_QT_MYACCOUNTDIALOG_HPP
#define ORES_QT_MYACCOUNTDIALOG_HPP

#include <QLabel>
#include <QWidget>
#include <QTabWidget>
#include <QToolBar>
#include <QLineEdit>
#include <QPushButton>
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

/**
 * @brief Widget for users to manage their own account details.
 *
 * Allows users to:
 * - View their username (read-only)
 * - View/edit their email address
 * - Change their password voluntarily
 * - View session activity
 *
 * This widget is opened as an MDI subwindow via File > My Account and is
 * available only when the user is logged in.
 */
class MyAccountDialog : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.my_account_dialog";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct MyAccountDialog.
     * @param clientManager Pointer to the application's client manager.
     * @param parent Parent widget.
     */
    explicit MyAccountDialog(ClientManager* clientManager, QWidget* parent = nullptr);
    ~MyAccountDialog() override;

private slots:
    void onChangePasswordClicked();
    void onChangePasswordResult(bool success, const QString& error_message);
    void onSaveEmailClicked();
    void onSaveEmailResult(bool success, const QString& error_message);
    void onViewSessionsClicked();

signals:
    void changePasswordCompleted(bool success, const QString& error_message);
    void saveEmailCompleted(bool success, const QString& error_message);
    void viewSessionHistoryRequested();

private:
    void setupUI();
    void loadAccountInfo();
    void loadSessionInfo();
    void enablePasswordForm(bool enabled);
    bool validatePasswordInput();

private:
    // Toolbar
    QToolBar* toolbar_;

    // Tab widget
    QTabWidget* tabWidget_;

    // Account info section (General tab)
    QLineEdit* username_edit_;
    QLineEdit* email_edit_;
    QPushButton* save_email_button_;
    QLabel* email_status_label_;

    // Sessions section (Sessions tab)
    QLabel* active_sessions_label_;
    QLabel* current_session_label_;
    QPushButton* view_sessions_button_;

    // Password change section (Security tab)
    QLineEdit* new_password_edit_;
    QLineEdit* confirm_password_edit_;
    QPushButton* change_password_button_;
    QLabel* password_status_label_;

    // Dependencies
    ClientManager* clientManager_;
};

}

#endif
