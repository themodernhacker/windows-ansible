# Windows Ansible Core (WAC)

Windows Ansible Core (WAC) is a Windows-native Ansible alternative implemented in Standard ML. It provides core Ansible functionality without requiring WSL, Linux control nodes, or Python dependencies.

## Features

- **Windows-native**: Runs directly on Windows without WSL or Linux control nodes
- **Ansible-compatible**: Uses familiar Ansible concepts (playbooks, tasks, modules)
- **PowerShell integration**: Seamless interaction with Windows PowerShell
- **Active Directory support**: Direct AD integration without additional modules
- **Idempotent operations**: Ensures changes are made only when necessary

## System Requirements

- Windows 10 or Windows Server 2016+
- Standard ML of New Jersey (SML/NJ) runtime
- PowerShell 5.1 or later

## Installation

1. **Install SML/NJ**:
   - Download from [SML/NJ website](https://www.smlnj.org/)
   - Add the SML/NJ bin directory to your PATH

2. **Clone this repository**: