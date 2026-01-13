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
#ifndef ORES_QT_MASTER_PASSWORD_DIALOG_HPP
#define ORES_QT_MASTER_PASSWORD_DIALOG_HPP

#include <QDialog>
#include <QLineEdit>
#include <QPushButton>
#include <QLabel>
#include <QDialogButtonBox>
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Dialog for entering or changing the master password.
 *
 * The master password is used to encrypt/decrypt saved passwords for
 * server environments. This dialog supports three modes:
 * - Unlock mode: Enter master password to access encrypted data
 * - Change mode: Change existing master password
 * - Create mode: Create new master password (first time)
 */
class MasterPasswordDialog : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.master_password_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Mode {
        Unlock,      // Enter password to unlock
        Change,      // Change existing password
        Create       // Create new master password (first time)
    };

    explicit MasterPasswordDialog(Mode mode, QWidget* parent = nullptr);
    ~MasterPasswordDialog() override;

    QString getPassword() const;
    QString getNewPassword() const;

private slots:
    void onOkClicked();
    void updatePasswordMatchIndicator();
    void updateOkButtonState();

private:
    void setupUI();
    bool validateInput();

    Mode mode_;

    QLabel* infoLabel_;
    QLineEdit* currentPasswordEdit_;
    QLineEdit* newPasswordEdit_;
    QLineEdit* confirmPasswordEdit_;
    QLabel* matchIndicatorLabel_;
    QDialogButtonBox* buttonBox_;
};

}

#endif
