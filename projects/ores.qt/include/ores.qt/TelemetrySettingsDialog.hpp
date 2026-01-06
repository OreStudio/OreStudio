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
#ifndef ORES_QT_TELEMETRYSETTINGSDIALOG_HPP
#define ORES_QT_TELEMETRYSETTINGSDIALOG_HPP

#include <QLabel>
#include <QDialog>
#include <QCheckBox>
#include <QComboBox>
#include <QGroupBox>
#include <QLineEdit>
#include <QSpinBox>
#include <QPushButton>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.telemetry/log/logging_options.hpp"

namespace ores::qt {

/**
 * @brief Dialog for configuring logging and telemetry settings.
 *
 * Allows users to configure:
 * - Logging: level, console output, file output, directory, PID inclusion, tag filter
 * - Telemetry Export: output file, directory, streaming, batch size, flush interval
 *
 * Settings are persisted via QSettings and applied on next application restart.
 * Some settings (like log level) may be applied immediately if the runtime supports it.
 *
 * This dialog is accessed via Telemetry > Settings... menu item.
 */
class TelemetrySettingsDialog : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.telemetry_settings_dialog";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct TelemetrySettingsDialog.
     * @param parent Parent widget.
     */
    explicit TelemetrySettingsDialog(QWidget* parent = nullptr);
    ~TelemetrySettingsDialog() override;

    /**
     * @brief Load logging options from QSettings.
     * @return Logging options struct populated from settings.
     */
    static telemetry::log::logging_options loadLoggingSettings();

    /**
     * @brief Get the settings key prefix for telemetry settings.
     * @return The settings key prefix.
     */
    static QString settingsPrefix() { return "telemetry"; }

private slots:
    void onApplyClicked();
    void onCancelClicked();
    void onBrowseLogDirectoryClicked();
    void onBrowseTelemetryDirectoryClicked();
    void onLoggingEnabledChanged(Qt::CheckState state);
    void onTelemetryExportEnabledChanged(Qt::CheckState state);

private:
    void setupUI();
    void loadSettings();
    void saveSettings();
    void applySettings();
    void updateLoggingGroupEnabled();
    void updateTelemetryGroupEnabled();

private:
    // Logging section
    QGroupBox* logging_group_;
    QCheckBox* logging_enabled_checkbox_;
    QComboBox* log_level_combo_;
    QCheckBox* console_output_checkbox_;
    QLineEdit* log_directory_edit_;
    QPushButton* log_directory_browse_;
    QLineEdit* log_filename_edit_;
    QCheckBox* include_pid_checkbox_;
    QLineEdit* tag_filter_edit_;

    // Telemetry export section
    QGroupBox* telemetry_group_;
    QCheckBox* telemetry_enabled_checkbox_;
    QLineEdit* telemetry_output_file_edit_;
    QLineEdit* telemetry_directory_edit_;
    QPushButton* telemetry_directory_browse_;
    QCheckBox* streaming_enabled_checkbox_;
    QSpinBox* batch_size_spin_;
    QSpinBox* flush_interval_spin_;

    // Dialog buttons
    QPushButton* apply_button_;
    QPushButton* cancel_button_;

    // Status
    QLabel* restart_hint_label_;
};

}

#endif
