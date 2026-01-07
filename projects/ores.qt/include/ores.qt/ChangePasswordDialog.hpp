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
#ifndef ORES_QT_CHANGEPASSWORDDIALOG_HPP
#define ORES_QT_CHANGEPASSWORDDIALOG_HPP

#include <QLabel>
#include <QDialog>
#include <QLineEdit>
#include <QPushButton>
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

/**
 * @brief Dialog for changing user password after forced password reset.
 *
 * Presents a form for entering new password with confirmation.
 * Used when the server indicates password_reset_required after login.
 */
class ChangePasswordDialog : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.change_password_dialog";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct ChangePasswordDialog.
     * @param clientManager Pointer to the application's client manager.
     * @param parent Parent widget.
     */
    explicit ChangePasswordDialog(ClientManager* clientManager, QWidget* parent = nullptr);
    ~ChangePasswordDialog() override;

private slots:
    void onChangeClicked();
    void onChangeResult(bool success, const QString& error_message);
    void updatePasswordMatchIndicator();

signals:
    void changeCompleted(bool success, const QString& error_message);

private:
    void setupUI();
    void enableForm(bool enabled);
    bool validateInput();

private:
    // UI components
    QLineEdit* new_password_edit_;
    QLineEdit* confirm_password_edit_;
    QPushButton* change_button_;
    QPushButton* cancel_button_;
    QLabel* status_label_;
    QLabel* info_label_;

    // Dependencies
    ClientManager* clientManager_;
};

}

#endif
