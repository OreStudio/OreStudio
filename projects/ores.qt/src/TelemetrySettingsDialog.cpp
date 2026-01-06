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
#include "ores.qt/TelemetrySettingsDialog.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QFileDialog>
#include <QSettings>
#include <QStandardPaths>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::telemetry::log;

TelemetrySettingsDialog::TelemetrySettingsDialog(QWidget* parent)
    : QDialog(parent),
      // Logging section
      logging_group_(new QGroupBox("Logging", this)),
      logging_enabled_checkbox_(new QCheckBox("Enable file logging", this)),
      log_level_combo_(new QComboBox(this)),
      console_output_checkbox_(new QCheckBox("Output to console", this)),
      log_directory_edit_(new QLineEdit(this)),
      log_directory_browse_(new QPushButton("Browse...", this)),
      log_filename_edit_(new QLineEdit(this)),
      include_pid_checkbox_(new QCheckBox("Include PID in filename", this)),
      tag_filter_edit_(new QLineEdit(this)),
      // Telemetry section
      telemetry_group_(new QGroupBox("Telemetry Export", this)),
      telemetry_enabled_checkbox_(new QCheckBox("Enable telemetry export", this)),
      telemetry_output_file_edit_(new QLineEdit(this)),
      telemetry_directory_edit_(new QLineEdit(this)),
      telemetry_directory_browse_(new QPushButton("Browse...", this)),
      streaming_enabled_checkbox_(new QCheckBox("Enable streaming to server", this)),
      batch_size_spin_(new QSpinBox(this)),
      flush_interval_spin_(new QSpinBox(this)),
      // Dialog buttons
      apply_button_(new QPushButton("Apply", this)),
      cancel_button_(new QPushButton("Cancel", this)),
      restart_hint_label_(new QLabel(this)) {

    setupUI();
    loadSettings();

    // Connect signals
    connect(apply_button_, &QPushButton::clicked,
            this, &TelemetrySettingsDialog::onApplyClicked);
    connect(cancel_button_, &QPushButton::clicked,
            this, &TelemetrySettingsDialog::onCancelClicked);
    connect(log_directory_browse_, &QPushButton::clicked,
            this, &TelemetrySettingsDialog::onBrowseLogDirectoryClicked);
    connect(telemetry_directory_browse_, &QPushButton::clicked,
            this, &TelemetrySettingsDialog::onBrowseTelemetryDirectoryClicked);
    connect(logging_enabled_checkbox_, &QCheckBox::checkStateChanged,
            this, &TelemetrySettingsDialog::onLoggingEnabledChanged);
    connect(telemetry_enabled_checkbox_, &QCheckBox::checkStateChanged,
            this, &TelemetrySettingsDialog::onTelemetryExportEnabledChanged);
}

TelemetrySettingsDialog::~TelemetrySettingsDialog() {
}

void TelemetrySettingsDialog::setupUI() {
    BOOST_LOG_SEV(lg(), debug) << "Setting up UI.";

    setWindowTitle("Telemetry Settings");
    setModal(true);
    setMinimumWidth(500);
    setFixedWidth(500);
    setSizeGripEnabled(false);

    const QColor iconColor(220, 220, 220);

    // Logging Group
    auto* logging_layout = new QFormLayout(logging_group_);
    logging_layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    // Enabled checkbox
    logging_layout->addRow(logging_enabled_checkbox_);

    // Log level
    log_level_combo_->addItems({"trace", "debug", "info", "warn", "error"});
    log_level_combo_->setCurrentText("info");
    logging_layout->addRow("Log Level:", log_level_combo_);

    // Console output
    logging_layout->addRow(console_output_checkbox_);

    // Log directory with browse button
    auto* log_dir_layout = new QHBoxLayout();
    log_directory_edit_->setPlaceholderText("Leave empty for current directory");
    log_dir_layout->addWidget(log_directory_edit_);
    log_directory_browse_->setIcon(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_folder_20_regular.svg", iconColor));
    log_directory_browse_->setFixedWidth(100);
    log_dir_layout->addWidget(log_directory_browse_);
    logging_layout->addRow("Log Directory:", log_dir_layout);

    // Filename
    log_filename_edit_->setPlaceholderText("e.g., ores-qt.log");
    logging_layout->addRow("Filename:", log_filename_edit_);

    // Include PID
    include_pid_checkbox_->setToolTip(
        "When enabled, the filename includes the process ID (e.g., ores-qt.12345.log)");
    logging_layout->addRow(include_pid_checkbox_);

    // Tag filter
    tag_filter_edit_->setPlaceholderText("Optional: filter logs by tag");
    tag_filter_edit_->setToolTip(
        "If set, only log messages with this tag will be written");
    logging_layout->addRow("Tag Filter:", tag_filter_edit_);

    // Telemetry Export Group
    auto* telemetry_layout = new QFormLayout(telemetry_group_);
    telemetry_layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    // Enabled checkbox
    telemetry_layout->addRow(telemetry_enabled_checkbox_);

    // Output file
    telemetry_output_file_edit_->setPlaceholderText("e.g., telemetry.jsonl");
    telemetry_layout->addRow("Output File:", telemetry_output_file_edit_);

    // Output directory with browse button
    auto* telem_dir_layout = new QHBoxLayout();
    telemetry_directory_edit_->setPlaceholderText("Leave empty for current directory");
    telem_dir_layout->addWidget(telemetry_directory_edit_);
    telemetry_directory_browse_->setIcon(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_folder_20_regular.svg", iconColor));
    telemetry_directory_browse_->setFixedWidth(100);
    telem_dir_layout->addWidget(telemetry_directory_browse_);
    telemetry_layout->addRow("Output Directory:", telem_dir_layout);

    // Streaming
    streaming_enabled_checkbox_->setToolTip(
        "When enabled, log records are also sent to the server in addition to local file");
    telemetry_layout->addRow(streaming_enabled_checkbox_);

    // Batch size
    batch_size_spin_->setRange(1, 1000);
    batch_size_spin_->setValue(50);
    batch_size_spin_->setToolTip("Number of records to batch before sending to server");
    telemetry_layout->addRow("Batch Size:", batch_size_spin_);

    // Flush interval
    flush_interval_spin_->setRange(1, 300);
    flush_interval_spin_->setValue(5);
    flush_interval_spin_->setSuffix(" seconds");
    flush_interval_spin_->setToolTip("Maximum time to wait before flushing a partial batch");
    telemetry_layout->addRow("Flush Interval:", flush_interval_spin_);

    // Restart hint
    restart_hint_label_->setText(
        "<i>Note: Most settings take effect on next application restart.</i>");
    restart_hint_label_->setStyleSheet("QLabel { color: #888; }");

    // Dialog buttons
    apply_button_->setIcon(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_checkmark_20_regular.svg", iconColor));
    cancel_button_->setIcon(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_dismiss_20_regular.svg", iconColor));

    auto* button_layout = new QHBoxLayout();
    button_layout->addWidget(restart_hint_label_);
    button_layout->addStretch();
    button_layout->addWidget(apply_button_);
    button_layout->addWidget(cancel_button_);

    // Main layout
    auto* main_layout = new QVBoxLayout(this);
    main_layout->addWidget(logging_group_);
    main_layout->addWidget(telemetry_group_);
    main_layout->addSpacing(10);
    main_layout->addLayout(button_layout);

    // Set default button
    apply_button_->setDefault(true);

    // Initial state update
    updateLoggingGroupEnabled();
    updateTelemetryGroupEnabled();
}

void TelemetrySettingsDialog::loadSettings() {
    BOOST_LOG_SEV(lg(), debug) << "Loading settings.";

    QSettings settings;
    settings.beginGroup(settingsPrefix());

    // Logging settings
    settings.beginGroup("logging");
    logging_enabled_checkbox_->setChecked(settings.value("enabled", false).toBool());
    log_level_combo_->setCurrentText(settings.value("level", "info").toString());
    console_output_checkbox_->setChecked(settings.value("console", false).toBool());
    log_directory_edit_->setText(settings.value("directory", "").toString());
    log_filename_edit_->setText(settings.value("filename", "ores-qt.log").toString());
    include_pid_checkbox_->setChecked(settings.value("include_pid", false).toBool());
    tag_filter_edit_->setText(settings.value("tag", "").toString());
    settings.endGroup();

    // Telemetry export settings
    settings.beginGroup("export");
    telemetry_enabled_checkbox_->setChecked(settings.value("enabled", false).toBool());
    telemetry_output_file_edit_->setText(settings.value("output_file", "telemetry.jsonl").toString());
    telemetry_directory_edit_->setText(settings.value("directory", "").toString());
    streaming_enabled_checkbox_->setChecked(settings.value("streaming", false).toBool());
    batch_size_spin_->setValue(settings.value("batch_size", 50).toInt());
    flush_interval_spin_->setValue(settings.value("flush_interval", 5).toInt());
    settings.endGroup();

    settings.endGroup();

    updateLoggingGroupEnabled();
    updateTelemetryGroupEnabled();
}

void TelemetrySettingsDialog::saveSettings() {
    BOOST_LOG_SEV(lg(), debug) << "Saving settings.";

    QSettings settings;
    settings.beginGroup(settingsPrefix());

    // Logging settings
    settings.beginGroup("logging");
    settings.setValue("enabled", logging_enabled_checkbox_->isChecked());
    settings.setValue("level", log_level_combo_->currentText());
    settings.setValue("console", console_output_checkbox_->isChecked());
    settings.setValue("directory", log_directory_edit_->text());
    settings.setValue("filename", log_filename_edit_->text());
    settings.setValue("include_pid", include_pid_checkbox_->isChecked());
    settings.setValue("tag", tag_filter_edit_->text());
    settings.endGroup();

    // Telemetry export settings
    settings.beginGroup("export");
    settings.setValue("enabled", telemetry_enabled_checkbox_->isChecked());
    settings.setValue("output_file", telemetry_output_file_edit_->text());
    settings.setValue("directory", telemetry_directory_edit_->text());
    settings.setValue("streaming", streaming_enabled_checkbox_->isChecked());
    settings.setValue("batch_size", batch_size_spin_->value());
    settings.setValue("flush_interval", flush_interval_spin_->value());
    settings.endGroup();

    settings.endGroup();
    settings.sync();
}

telemetry::log::logging_options TelemetrySettingsDialog::loadLoggingSettings() {
    QSettings settings;
    settings.beginGroup(settingsPrefix());
    settings.beginGroup("logging");

    logging_options opts;
    opts.severity = settings.value("level", "info").toString().toStdString();
    opts.output_to_console = settings.value("console", false).toBool();
    opts.output_directory = settings.value("directory", "").toString().toStdString();
    opts.filename = settings.value("filename", "").toString().toStdString();
    opts.include_pid = settings.value("include_pid", false).toBool();
    opts.tag = settings.value("tag", "").toString().toStdString();

    // Only set filename if logging is enabled
    if (!settings.value("enabled", false).toBool()) {
        opts.filename.clear();
    }

    settings.endGroup();
    settings.endGroup();

    return opts;
}

void TelemetrySettingsDialog::applySettings() {
    BOOST_LOG_SEV(lg(), info) << "Applying telemetry settings.";
    saveSettings();
}

void TelemetrySettingsDialog::onApplyClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Apply button clicked.";
    applySettings();

    MessageBoxHelper::information(this, "Settings Saved",
        "Telemetry settings have been saved.\n\n"
        "Most changes will take effect when you restart the application.");

    accept();
}

void TelemetrySettingsDialog::onCancelClicked() {
    BOOST_LOG_SEV(lg(), trace) << "Cancel button clicked.";
    reject();
}

void TelemetrySettingsDialog::onBrowseLogDirectoryClicked() {
    BOOST_LOG_SEV(lg(), trace) << "Browse log directory clicked.";

    QString current_dir = log_directory_edit_->text();
    if (current_dir.isEmpty()) {
        current_dir = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
    }

    QString dir = QFileDialog::getExistingDirectory(
        this, "Select Log Directory", current_dir,
        QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks);

    if (!dir.isEmpty()) {
        log_directory_edit_->setText(dir);
    }
}

void TelemetrySettingsDialog::onBrowseTelemetryDirectoryClicked() {
    BOOST_LOG_SEV(lg(), trace) << "Browse telemetry directory clicked.";

    QString current_dir = telemetry_directory_edit_->text();
    if (current_dir.isEmpty()) {
        current_dir = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
    }

    QString dir = QFileDialog::getExistingDirectory(
        this, "Select Telemetry Output Directory", current_dir,
        QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks);

    if (!dir.isEmpty()) {
        telemetry_directory_edit_->setText(dir);
    }
}

void TelemetrySettingsDialog::onLoggingEnabledChanged(Qt::CheckState /*state*/) {
    updateLoggingGroupEnabled();
}

void TelemetrySettingsDialog::onTelemetryExportEnabledChanged(Qt::CheckState /*state*/) {
    updateTelemetryGroupEnabled();
}

void TelemetrySettingsDialog::updateLoggingGroupEnabled() {
    bool enabled = logging_enabled_checkbox_->isChecked();

    log_level_combo_->setEnabled(true);  // Level always available for console
    console_output_checkbox_->setEnabled(true);
    log_directory_edit_->setEnabled(enabled);
    log_directory_browse_->setEnabled(enabled);
    log_filename_edit_->setEnabled(enabled);
    include_pid_checkbox_->setEnabled(enabled);
    tag_filter_edit_->setEnabled(true);  // Tag filter can work with console too
}

void TelemetrySettingsDialog::updateTelemetryGroupEnabled() {
    bool enabled = telemetry_enabled_checkbox_->isChecked();

    telemetry_output_file_edit_->setEnabled(enabled);
    telemetry_directory_edit_->setEnabled(enabled);
    telemetry_directory_browse_->setEnabled(enabled);
    streaming_enabled_checkbox_->setEnabled(enabled);
    batch_size_spin_->setEnabled(enabled && streaming_enabled_checkbox_->isChecked());
    flush_interval_spin_->setEnabled(enabled && streaming_enabled_checkbox_->isChecked());
}

}
